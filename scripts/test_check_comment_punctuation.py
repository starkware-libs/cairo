import unittest

from check_comment_punctuation import check_file, extract_comment_text, is_doc_comment


class DocCommentTests(unittest.TestCase):
    def test_four_slashes_are_an_ordinary_rust_comment(self):
        self.assertFalse(is_doc_comment("    //// An ordinary comment"))
        self.assertEqual(extract_comment_text("//// An ordinary comment"), "")
        self.assertEqual(check_file(["//// An ordinary comment\n", "fn example() {}\n"]), [])

    def test_real_doc_comments_still_require_punctuation(self):
        for prefix in ("///", "//!"):
            with self.subTest(prefix=prefix):
                self.assertTrue(is_doc_comment(prefix + " Documentation"))
                self.assertEqual(len(check_file([prefix + " Documentation\n"])), 1)
                self.assertEqual(check_file([prefix + " Documentation.\n"]), [])


if __name__ == "__main__":
    unittest.main()
