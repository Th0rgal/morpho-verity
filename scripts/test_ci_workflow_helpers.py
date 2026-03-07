#!/usr/bin/env python3
"""Unit tests for shared verify.yml parsing helpers."""

from __future__ import annotations

import pathlib
import sys
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from ci_workflow_helpers import strip_yaml_comment, strip_yaml_scalar


class StripYamlCommentTests(unittest.TestCase):
  def test_strips_unquoted_trailing_comments(self) -> None:
    self.assertEqual(strip_yaml_comment("value # comment"), "value")

  def test_preserves_plain_hash_without_comment_whitespace(self) -> None:
    self.assertEqual(strip_yaml_comment("value#fragment"), "value#fragment")

  def test_preserves_plain_hash_followed_by_space_without_comment_whitespace(self) -> None:
    self.assertEqual(strip_yaml_comment("value# fragment"), "value# fragment")

  def test_preserves_hash_inside_double_quotes(self) -> None:
    self.assertEqual(strip_yaml_comment('"value#still-value" # comment'), '"value#still-value"')

  def test_preserves_hash_inside_single_quotes(self) -> None:
    self.assertEqual(strip_yaml_comment("'value#still-value' # comment"), "'value#still-value'")


class StripYamlScalarTests(unittest.TestCase):
  def test_unquotes_and_strips_comment(self) -> None:
    self.assertEqual(strip_yaml_scalar(' "value" # comment'), "value")

  def test_preserves_plain_hash_scalar(self) -> None:
    self.assertEqual(strip_yaml_scalar("artifact#bundle"), "artifact#bundle")

  def test_preserves_hash_inside_quoted_scalar(self) -> None:
    self.assertEqual(strip_yaml_scalar('"value#still-value"'), "value#still-value")


if __name__ == "__main__":
  unittest.main()
