(org-data
 (:begin 1 :contents-begin 2 :contents-end 1306 :end 1306 :post-affiliated 1 :post-blank 0)
 (section
  (:begin 2 :contents-begin 2 :contents-end 837 :end 838 :post-affiliated 2 :post-blank 1)
  (paragraph
   (:begin 2 :contents-begin 2 :contents-end 51 :end 52 :post-affiliated 2 :post-blank 1)
   "This is a shared test suite for Org mode syntax.\n")
  (paragraph
   (:begin 52 :contents-begin 52 :contents-end 247 :end 248 :post-affiliated 52 :post-blank 1)
   "The test suite consists of a number of .org example files alongside\nwith the expected parser output.  Each .org file can be parsed as is\nand the result should match the corresponding .el file.  \n")
  (paragraph
   (:begin 248 :contents-begin 248 :contents-end 424 :end 425 :post-affiliated 248 :post-blank 1)
   "The parser results in .el files are Emacs sexps.  Each sexp is an\noutput of "
   (verbatim
    (:begin 324 :end 351 :post-blank 1))
   "stripped from unessential\nproperties.  Each sexp has the following form:\n")
  (src-block
   (:begin 425 :end 773 :post-affiliated 425 :post-blank 1))
  (paragraph
   (:begin 773 :contents-begin 773 :contents-end 837 :end 837 :post-affiliated 773 :post-blank 0)
   "The properties of elements can be specified in arbitrary order.\n"))
 (headline
  (:archivedp nil :begin 838 :commentedp nil :contents-begin 854 :contents-end 1306 :end 1306 :footnote-section-p nil :level 1 :post-affiliated 838 :post-blank 0 :pre-blank 1 :priority nil :raw-value "Contributing" :tags nil :title
	      ("Contributing")
	      :todo-keyword nil :todo-type nil)
  (section
   (:begin 854 :contents-begin 854 :contents-end 1306 :end 1306 :post-affiliated 854 :post-blank 0)
   (paragraph
    (:begin 854 :contents-begin 854 :contents-end 983 :end 984 :post-affiliated 854 :post-blank 1)
    "To add new test files to this suite, send a patch to Org mode mailing\nlist, as described in "
    (link
     (:begin 946 :contents-begin nil :contents-end nil :end 981 :post-blank 0))
    ".\n")
   (paragraph
    (:begin 984 :contents-begin 984 :contents-end 1306 :end 1306 :post-affiliated 984 :post-blank 0)
    "The expected parser output can be generated using Emacs and latest\nversion of Org mode.  You need to open an Org file in Emacs, load\n"
    (verbatim
     (:begin 1117 :end 1180 :post-blank 0))
    ", and\nrun "
    (verbatim
     (:begin 1190 :end 1240 :post-blank 0))
    ".  The expected\noutput will be saved alongside with the Org file.\n"))))
