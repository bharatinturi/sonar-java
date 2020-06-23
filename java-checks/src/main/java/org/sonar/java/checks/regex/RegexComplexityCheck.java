/*
 * SonarQube Java
 * Copyright (C) 2012-2020 SonarSource SA
 * mailto:info AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.java.checks.regex;

import java.util.ArrayList;
import java.util.List;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.java.model.ExpressionUtils;
import org.sonar.java.regex.RegexParseResult;
import org.sonar.java.regex.ast.AtomicGroupTree;
import org.sonar.java.regex.ast.BackReferenceTree;
import org.sonar.java.regex.ast.CapturingGroupTree;
import org.sonar.java.regex.ast.CharacterClassIntersectionTree;
import org.sonar.java.regex.ast.CharacterClassTree;
import org.sonar.java.regex.ast.DisjunctionTree;
import org.sonar.java.regex.ast.FlagSet;
import org.sonar.java.regex.ast.LookAroundTree;
import org.sonar.java.regex.ast.NonCapturingGroupTree;
import org.sonar.java.regex.ast.RegexBaseVisitor;
import org.sonar.java.regex.ast.RegexTree;
import org.sonar.java.regex.ast.RepetitionTree;
import org.sonar.plugins.java.api.tree.BinaryExpressionTree;
import org.sonar.plugins.java.api.tree.ExpressionTree;
import org.sonar.plugins.java.api.tree.IdentifierTree;
import org.sonar.plugins.java.api.tree.LiteralTree;
import org.sonar.plugins.java.api.tree.MethodInvocationTree;
import org.sonar.plugins.java.api.tree.Tree;

@Rule(key = "S5843")
public class RegexComplexityCheck extends AbstractRegexCheck {

  private static final String MESSAGE = "Simplify this regular expression to reduce its complexity from %d to the %d allowed.";

  private static final int DEFAULT_MAX = 15;

  @RuleProperty(
    key = "maxComplexity",
    description = "The maximum authorized complexity.",
    defaultValue = "" + DEFAULT_MAX)
  private int max = DEFAULT_MAX;

  @Override
  protected void checkRegexConstruction(ExpressionTree regexArgument, FlagSet flags, MethodInvocationTree mit) {
    for (LiteralTree[] regexPart : findRegexParts(regexArgument)) {
      checkRegex(regexForLiterals(flags, regexPart), mit);
    }
  }

  @Override
  public void checkRegex(RegexParseResult regexForLiterals, MethodInvocationTree mit) {
    new ComplexityCalculator().visit(regexForLiterals);
  }

  List<LiteralTree[]> findRegexParts(ExpressionTree regexArgument) {
    RegexPartFinder finder = new RegexPartFinder();
    finder.find(regexArgument);
    return finder.parts;
  }

  public void setMax(int max) {
    this.max = max;
  }

  private static class RegexPartFinder {

    List<LiteralTree[]> parts = new ArrayList<>();

    void find(ExpressionTree expr) {
      switch (expr.kind()) {
        case PLUS:
          List<LiteralTree> literals = new ArrayList<>();
          findInStringConcatenation(expr, literals);
          if (!literals.isEmpty()) {
            parts.add(literals.toArray(new LiteralTree[0]));
          }
          break;
        case IDENTIFIER:
          getFinalVariableInitializer((IdentifierTree) expr).ifPresent(this::find);
          break;
        case PARENTHESIZED_EXPRESSION:
          find(ExpressionUtils.skipParentheses(expr));
          break;
        case STRING_LITERAL:
          parts.add(new LiteralTree[] {(LiteralTree) expr});
          break;
        default:
          // Do nothing
      }
    }

    void findInStringConcatenation(ExpressionTree expr, List<LiteralTree> literals) {
      if (expr.is(Tree.Kind.STRING_LITERAL)) {
        literals.add((LiteralTree) expr);
      } else if (expr.is(Tree.Kind.PLUS)) {
        BinaryExpressionTree binExpr = (BinaryExpressionTree) expr;
        findInStringConcatenation(binExpr.leftOperand(), literals);
        findInStringConcatenation(binExpr.rightOperand(), literals);
      } else if (expr.is(Tree.Kind.PARENTHESIZED_EXPRESSION)) {
        findInStringConcatenation(ExpressionUtils.skipParentheses(expr), literals);
      } else {
        find(expr);
      }
    }

  }

  private class ComplexityCalculator extends RegexBaseVisitor {

    int complexity = 0;

    int nesting = 1;

    List<RegexIssueLocation> components = new ArrayList<>();

    private void increaseComplexity(RegexTree tree, int increment) {
      complexity += increment;
      components.add(new RegexIssueLocation(tree, "+" + increment));
    }

    @Override
    public void visitDisjunction(DisjunctionTree tree) {
      int size = tree.getAlternatives().size();
      increaseComplexity(tree, nesting + size - 2);
      super.visitDisjunction(tree);
    }

    @Override
    public void visitRepetition(RepetitionTree tree) {
      increaseComplexity(tree, nesting);
      super.visitRepetition(tree);
    }

    // Character classes increase the complexity by only one regardless of nesting because they're not that complex by
    // themselves
    @Override
    public void visitCharacterClass(CharacterClassTree tree) {
      increaseComplexity(tree, 1);
      nesting++;
      super.visitCharacterClass(tree);
      nesting--;
    }

    // Intersections in character classes are a different matter though
    @Override
    public void visitCharacterClassIntersection(CharacterClassIntersectionTree tree) {
      increaseComplexity(tree, nesting);
      nesting++;
      super.visitCharacterClassIntersection(tree);
      nesting--;
    }

    // Regular groups, names groups and non-capturing groups without flags don't increase complexity because they don't
    // do anything by themselves. However lookarounds, atomic groups and non-capturing groups with flags do because
    // they're more complicated features
    @Override
    public void visitCapturingGroup(CapturingGroupTree tree) {
      nesting++;
      super.visitCapturingGroup(tree);
      nesting--;
    }

    @Override
    protected void doVisitNonCapturingGroup(NonCapturingGroupTree tree) {
      if (!tree.getEnabledFlags().isEmpty() || !tree.getDisabledFlags().isEmpty()) {
        increaseComplexity(tree, nesting);
      }
      if (tree.getElement() == null) {
        return;
      }
      nesting++;
      super.doVisitNonCapturingGroup(tree);
      nesting--;
    }

    @Override
    public void visitLookAround(LookAroundTree tree) {
      increaseComplexity(tree, nesting);
      nesting++;
      super.visitLookAround(tree);
      nesting--;
    }

    @Override
    public void visitAtomicGroup(AtomicGroupTree tree) {
      increaseComplexity(tree, nesting);
      nesting++;
      super.visitAtomicGroup(tree);
      nesting--;
    }

    @Override
    public void visitBackReference(BackReferenceTree tree) {
      complexity++;
    }

    @Override
    protected void after(RegexParseResult regexParseResult) {
      if (complexity > max) {
        reportIssue(regexParseResult.getResult(), String.format(MESSAGE, complexity, max), complexity - max , components);
      }
    }
  }

}
