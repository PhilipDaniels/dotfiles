;;; Coding enhancement for C++ mode
;;; Usage:  (require 'pd-cpp)
;;;
;;; pd-cpp provides functions that enhance the experience of coding C++. The main
;;; feature is the ability to automatically look up the standard header file that
;;; a symbol resides in and add a #include<...> statement at the top of the file.
;;; Furthermore, a 'using' statement will be added, but only in .cpp files
;;; because it is bad practice to do so in header files.

(eval-when-compile (require 'pd))
(pd-log-requires-complete)

(defvar pd-cpp-use-std-namespace t
  "If non-nil, inserts 'using namespace std;' instead of 'using std::symbol;'")

(defvar pd-cpp-header-file-regex ".*\\.\\(h\\|hpp\\|hxx\\|hh\\)$"
  "A regular expression to match any C/C++ header file name.")

(defvar pd-cpp-auto-include-characters "(<&* "
  "A list of characters which trigger automatic insertion of headers
and using statements for the previous symbol. NOT USED.")

(defvar pd-cpp-auto-cleanup t
  "If non-nil, C++ buffers will be automatically cleaned up before saving.")

(defvar pd-cpp-include-map
  '(
    ;; The first element is the name of the header to be used in #include <..>
    ;; and the subsequent elements are the names of symbols that appear in that
    ;; header.
    ("algorithm" "all_of" "any_of" "none_of" "for_each" "count" "count_if"
     "mismatch" "equal" "find" "find_if" "find_if_not"
     "find_end" "find_first_of" "adjacent_find"
     "search" "search_n"
     "copy" "copy_if" "copy_n" "copy_backward" "move_backward"
     "fill" "fill_n" "transform" "generate" "generate_n"
     "remove" "remove_if" "remove_copy" "remove_copy_if"
     "replace" "replace_if" "replace_copy" "replace_copy_if"
     "swap_ranges" "iter_swap" "reverse" "reverse_copy"
     "rotate" "rotate_copy" "shuffle" "unique" "unique_copy"
     "is_partioned" "partition" "partition_copy"
     "stable_partition" "partition_point"
     "is_sorted" "is_sorted_until" "sort" "partial_sort"
     "partial_sort_copy" "stable_sort" "nth_element"
     "lower_bound" "upper_bound" "binary_search" "equal_range"
     "merge" "inplace_merge" "includes" "set_difference"
     "set_intersection" "set_symmetric_difference" "set_union"
     "is_heap" "is_heap_until" "make_heap" "push_heap"
     "pop_heap" "sort_heap"
     "max" "max_element" "min" "min_element" "minmax"
     "minmax_element" "lexicographical_compare"
     "is_permutation" "next_permutation" "prev_permutation")
    ("array" "array")
    ("cassert" "assert")
    ("cctype" "isalnum" "isalpha" "islower" "isupper" "isdigit" "isxdigit"
     "iscntrl" "isgraph" "isspace" "isblank" "isprint" "ispunct"
     "tolower" "toupper")
    ("cmath" "INFINITY" "NAN" "float_t" "double_t" "abs" "fabs"
     "fmod" "remainder" "remquo" "fma" "fmax" "fmin" "fdim" "nan"
     "nanf" "nanl" "exp" "exp2" "expm1" "log" "log10" "log2" "log1p"
     "pow" "sqrt" "cbrt" "hypot" "sin" "cos" "tan" "asin" "acos"
     "atan" "atan2" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh"
     "erf" "erfc" "tgamma" "lgamma" "ceil" "floor" "trunc" "round"
     "lround" "llround" "nearbyint" "rint" "lrint" "llrint"
     "frexp" "ldexp" "modf" "scalbn" "scalbln" "ilogb" "logb"
     "nextafter" "nexttoward" "fpclassify" "isfinite" "isinf" "isnan"
     "isnormal" "signbit" "isgreater" "isgreaterequal" "isless"
     "islessequal" "islessgreater" "isunordered")
    ("complex" "complex" "real" "imag" "norm" "conj" "proj" "polar")
    ("cstddef" "offsetof" "size_t" "ptrdiff_t" "nullptr_t" "max_align_t")
    ("cstdint" "int8_t" "int16_t" "int32_t" "int64_t"
     "uint8_t" "uint16_t" "uint32_t" "uint64_t"
     "intmax_t" "intptr_t" "uintmax_t" "uintptr_t")
    ("cstdio" "NULL ""stdin" "stdout" "stderr" "EOF" "FOPEN_MAX" "FILENAME_MAX"
     "BUFSIZ" "SEEK_SET" "SEEK_CUR" "SEEK_END" "TMP_MAX" "fopen" "freopen"
     "fclose" "fflush" "setbuf" "setvbuf" "fread" "fwrite" "fgetc" "getc"
     "fgets" "fputc" "putc" "fputs" "getchar" "gets" "putchar" "puts"
     "ungetc" "scanf" "fscanf" "sscanf" "vscanf" "vfscanf" "vsscanf"
     "printf" "fprintf" "sprintf" "snprintf" "vprintf" "vfprintf"
     "vsprintf" "vsnprintf" "ftell" "fgetpos" "fseek" "fsetpos" "rewind"
     "clearerr" "feof" "ferror" "perror" "rename" "tmpfile" "tmpnam"
     ;; don't include the remove function because it is also in algorithm.
     )
    ("deque" "deque")
    ("forward_list" "forward_list")
    ("fstream" "basic_filebuf" "basic_ifstream" "basic_ofstream" "basic_fstream"
     "filebuf" "wfilebuf" "ifstream" "wifstream" "ofstream"
     "wofstream" "fstream" "wfstream")
    ("functional" "mem_fn" "bad_function_call" "is_bind_expression"
     "is_placeholder" "reference_wrapper" "bind" "hash" "function"
     "plus" "minus" "multiplies" "divides" "modulus" "negate"
     "equal_to" "not_equal_to"
     "greater" "less" "greater_equal" "less_equal"
     "logical_and" "logical_or" "logical_not"
     "bit_and" "bit_or" "bit_xor"
     "unary_negate" "binary_negate" "not1" "not2")
    ("initializer_list" "initializer_list")
    ("iomanip" "resetiosflags" "setiosflags" "setbase" "setfill"
     "setprecision" "setw" "get_money" "put_money" "get_time"
     "put_time" "quoted")
    ("ios" "ios_base" "basic_ios" "fpos" "io_errc" "iostream_category"
     "is_error_code_enum" "streamoff" "streamsize" "openmode"
     "make_error_code" "make_error_condition"
     "boolalpha" "noboolalpha"
     "showbase" "noshowbase" "showpoint" "noshowpoint"
     "showpos" "noshowpos" "skipws" "noskipws"
     "uppercase" "nouppercase" "unitbuf" "nounitbuf"
     "internal" "left" "right" "dec" "hex" "oct"
     "fixed" "scientific" "hexfloat" "defaultfloat")
    ("iostream" "cin" "cout" "cerr" "clog" "wcout" "wcin" "wcerr" "wclog")
    ("istream" "basic_istream" "istream" "wistream" "basic_iostream"
     "iostream" "wiostream" "ws")
    ("iterator" "iterator_traits" "input_iterator_tag" "output_iterator_tag"
     "forward_iterator_tag" "bidirectional_iterator_tag"
     "random_access_iterator_tag"
     "iterator" "reverse_iterator"
     "move_iterator" "back_insert_iterator"
     "front_insert_iterator" "insert_iterator"
     "istream_iterator" "ostream_iterator"
     "istreambuf_iterator" "ostreambuf_iterator"
     "make_reverse_iterator" "make_move_iterator"
     "front_inserter" "back_inserter" "inserter"
     "advance" "distance" "next" "prev" "begin" "cbegin"
     "end" "cend" "rbegin" "crbegin" "rend" "crend")
    ("list" "list")
    ("map" "map" "multimap")
    ("memory" "unique_ptr" "shared_ptr" "weak_ptr" "make_shared" "addressof")
    ("mutex" "mutex" "timed_mutex" "recursive_mutex" "recursive_timed_mutex"
     "lock_guard" "unique_lock" "defer_lock_t" "try_to_lock_t"
     "adopt_lock_t" "defer_lock" "try_to_lock" "adopt_lock"
     "once_flag" "try_lock" "lock" "call_once")
    ("numeric" "iota" "accumulate" "inner_product" "adjacent_difference"
     "partial_sum")
    ("ostream" "basic_ostream" "ostream" "wostream" "ends" "flush" "endl")
    ("queue" "queue" "priority_queue")
    ("ratio" "ratio" "ratio_add" "ratio_subtract" "ratio_multiply"
     "ratio_divide" "yocto" "zepto" "atto" "femto" "pico" "nano"
     "micro" "milli" "centi" "deci" "deca" "hecto" "kilo" "mega"
     "giga" "tera" "peta" "exa" "zetta" "yotta")
    ("regex" "regex" "smatch" "ssub_match" "sregex_iterator"
     "scregex_token_iterator" "cmatch" "csub_match" "cregex_iterator"
     "cregex_token_iterator" "wregex" "wsmatch" "wssub_match" "wsregex_iterator"
     "wcmatch" "wcsub_match" "wcregex_iterator" "wcregex_token_iterator"
     "basic_regex" "sub_match" "match_results" "regex_match"
     "regex_search" "regex_replace" "regex_iterator"
     "regex_token_iterator" "regex_error" "regex_traits"
     ;; These constants are actually in std::regex_constants::xxx.
     "match_default" "match_not_bol" "match_not_eol" "match_not_bow"
     "match_not_eow" "match_any" "match_not_null" "match_continuous"
     "match_prev_avail" "format_default" "format_sed"
     "format_no_copy" "format_first_only")
    ("set" "set" "multiset")
    ("shared_mutex" "shared_mutex" "shared_timed_mutex" "shared_lock")
    ("sstream" "basic_stringbuf" "basic_istringstream"
     "basic_ostringstream" "basic_stringstream"
     "stringbuf" "wstringbuf" "istringstream" "wistringstream"
     "ostringstream" "wostringstream"
     "stringstream" "wstringstream")
    ("stack" "stack")
    ("stdexcept" "logic_error" "invalid_argument" "domain_error"
     "length_error" "out_of_range" "runtime_error" "range_error"
     "overflow_error" "underflow_error")
    ("streambuf" "basic_streambuf" "streambuf" "wstreambuf")
    ("string" "char_traits" "basic_string" "string" "wstring"
     "u16string" "u32string" "getline" "stoi" "stol" "stoll" "stoul"
     "stoull" "stof" "stod" "stold" "to_string" "to_wstring")
    ("thread" "thread" "yield" "get_id" "sleep_for" "sleep_until")
    ("tuple" "tuple" "make_tuple" "get")
    ("unordered_map" "unordered_map" "unordered_multimap")
    ("unordered_set" "unordered_set" "unordered_multiset")
    ("utility" "pair" "make_pair" "swap" "move" "exchange" "forward")
    ("vector" "vector")
    )
  "Map words such as 'cout' and 'vector' to the C++ standard header used to
#include them. These definitions are used in cpp files, and header files
unless there is an override defined in PD-CPP-HEADER-INCLUDE-MAP."
  )

(defvar pd-cpp-header-include-map
  '(
    ;; The first element is the name of the header to be used in #include <..>
    ;; and the subsequent elements are the names of symbols that appear in that
    ;; header.
    ("iosfwd" "char_traits" "allocator" "basic_ios" "basic_streambuf"
     "basic_istream" "basic_ostream" "basic_iostream" "basic_stringbuf"
     "basic_istringstream" "basic_ostringstream" "basic_stringstream"
     "basic_filebuf" "basic_ifstream" "basic_ofstream" "basic_fstream"
     "istreambuf_iterator" "ostreambuf_iterator" "ios" "wios"
     "streambuf" "istream" "ostream" "iostream" "stringbuf" "istringstream"
     "ostringstream" "stringstream" "filebuf" "ifstream" "ofstream" "fstream"
     "wstreambuf" "wistream" "wostream" "wiostream" "wstringbuf"
     "wistringstream" "wostringstream" "wstringstream" "wfilebuf" "wifstream"
     "wofstream" "wfstream" "fpos" "streampos" "wstreampos")
    )
  "Map words such as 'istream' to the C++ header used to #include them. These
definitions are used in header files, and override those from
PD-CPP-INCLUDE-MAP."
  )

(defun pd-cpp-find-first-paragraph-starting (s)
  "Returns the (BEG . END) point of the first paragraph, if any, that starts
with the specified string S. The paragraph is guaranteed to be outside a
comment.

For some reason this function tends to return leading whitespace. I consider
this to be a bug."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p (re-search-forward s nil t)))
      (when p
        (bounds-of-thing-at-point 'paragraph)))))

(defun pd-cpp-cmp-includes (s1 s2)
  "Compare C-style include statements, ensuring #include <...>
appears before #include \"...\".

S1 and S2 are the two strings (lines) to compare."
  ;; Deal with any whacky spacing by just getting rid of all whitespace.
  (setq s1 (replace-regexp-in-string "[[:space:]]" "" s1))
  (setq s2 (replace-regexp-in-string "[[:space:]]" "" s2))
  ;; Replace " with a character - the question mark - that sorts after it but
  ;; that will never show up in a filename.
  (setq s1 (replace-regexp-in-string "\"" "\?" s1))
  (setq s2 (replace-regexp-in-string "\"" "\?" s2))
  ;; (message "Comparing strings %s and %s" s1 s2)
  (string< s1 s2))

(defun pd-cpp-cmp-usings (s1 s2)
  "Compare CPP using statements, ensuring 'using namespace ...'
appears after using 'std::cout' statements.

S1 and S2 are the two strings (lines) to compare."
  ;; Deal with any whacky spacing by just getting rid of all whitespace.
  (setq s1 (replace-regexp-in-string "[[:space:]]" "" s1))
  (setq s2 (replace-regexp-in-string "[[:space:]]" "" s2))
  ;; Replace 'using namespace' with something that will sort after 'using'.
  (setq s1 (replace-regexp-in-string "usingnamespace" "zzz" s1))
  (setq s2 (replace-regexp-in-string "usingnamespace" "zzz" s2))
  ;; (message "Comparing strings %s and %s" s1 s2)
  (string< s1 s2))

(defun pd-cpp-lookup-symbol-header (cpp-symbol include-map)
  "Returns the appropriate C++ header file for a symbol, or nil if
CPP-SYMBOL is not a known symbol."
  (car (-first
        (lambda (cpp-mapping)
          (member cpp-symbol (cdr cpp-mapping)))
        include-map)))

(defun pd-cpp-add-include-and-sort (include-stmt)
  "Utility function to add a #include statement. Checks for duplicates
before inserting and sorts the #include block. If the file starts with
#pragma once or #ifndef...#define, the insertion occurs after them, which
makes it work correctly in header files."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward include-stmt nil t)
      (let ((insertion-point (cdr (pd-cpp-find-first-paragraph-starting "^#include"))))
        (unless insertion-point
          (goto-char (point-min))
          (setq insertion-point (re-search-forward "#pragma once.*$\\|#ifndef .*\n#define .*$" 1000 t))
          (cond (insertion-point
                 (insert "\n\n")
                 (setq insertion-point (+ insertion-point 2)))
                (t
                 (setq insertion-point (point-min))))
          )
        (goto-char insertion-point)
        (insert include-stmt "\n")
        (pd-cpp-sort-includes)
        ))))

(defun pd-cpp-add-using-and-sort (using-stmt)
  "Utility function to add a using statement. Checks for duplicates
and sorts the using block."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward using-stmt nil t)
      (let ((insertion-point (cdr (pd-cpp-find-first-paragraph-starting "^using "))))
        (cond (insertion-point
               (goto-char insertion-point)
               (insert using-stmt "\n"))
              (t
               (setq insertion-point (cdr (pd-cpp-find-first-paragraph-starting "^#include")))
               (goto-char insertion-point)
               (insert "\n" using-stmt "\n")))
        (pd-cpp-sort-usings)
        ))))

(defun pd-cpp-header-p (&optional buf-name)
  "Returns non-nil if BUFFER-NAME is a C/C++ header file, nil otherwise.
BUFFER-NAME defaults to the current buffer."
  (or buf-name (setq buf-name (buffer-name)))
  (let ((case-fold-search t))
    (string-match pd-cpp-header-file-regex buf-name)))

;;;###autoload
(defun pd-cpp-sort-includes ()
  "Sort the #include block at the beginning of a file. The cursor does not
have to be in the paragraph."
  (interactive)
  (let ((bounds (pd-cpp-find-first-paragraph-starting "^#include")))
    (if bounds
        (pd-sort-lines nil (car bounds) (cdr bounds) 'pd-cpp-cmp-includes))))

;;;###autoload
(defun pd-cpp-sort-usings ()
  "Sort the using statements at the beginning of a C++ file. The cursor does not
have to be in the paragraph."
  (interactive)
  (let ((bounds (pd-cpp-find-first-paragraph-starting "^using ")))
    (if bounds
        (pd-sort-lines nil (car bounds) (cdr bounds) 'pd-cpp-cmp-usings))))

;;;###autoload
(defun pd-cpp-add-using ()
  "Adds a using statement, and possibly a #include, for the C++ word at point."
  (interactive)
  ;; Because this function can be called automatically when typing certain
  ;; commonly occuring characters, optimize for early exit if there is no
  ;; match for a symbol.
  (let ((w (thing-at-point 'symbol t)))
    (when w
      (let ((main-include (pd-cpp-lookup-symbol-header w pd-cpp-include-map)))
        (when main-include
          ;; If w is a symbol defined in pd-cpp-header-include-map and we are in
          ;; a header file, favour including that header instead.
          (let* ((hdr-include (if (pd-cpp-header-p)
                                  (pd-cpp-lookup-symbol-header w pd-cpp-header-include-map)))
                 (include-stmt (concat "#include <" (or hdr-include main-include) ">"))
                 (using-stmt (if pd-cpp-use-std-namespace "using namespace std;"
                               (concat "using std::" w ";"))))
            ;; Always add the #include first, so that there will be a #include
            ;; block in existence when we come to add the using statement. Don't
            ;; add using statements in headers.
            (pd-cpp-add-include-and-sort include-stmt)
            (unless (pd-cpp-header-p)
              (pd-cpp-add-using-and-sort using-stmt))
            ))))))

(defun pd-cpp-cleanup-buffer ()
  "Runs various cleanups; recommended for programming modes only.

Also not recommended when working with other people's code
because it will re-indent the entire buffer."
  (interactive)
  (when (derived-mode-p 'c++-mode)
    (pd-indent-buffer)
    (pd-untabify-buffer)
    (delete-trailing-whitespace)
    (pd-cpp-sort-includes)
    (pd-cpp-sort-usings)
    ))

(defun pd-cpp-cleanup-buffer-maybe ()
  "Cleans up the current buffer if PD-CPP-AUTO-CLEANUP is non-nil."
  (when pd-cpp-auto-cleanup
    (pd-cpp-cleanup-buffer)))

(defun pd-cpp-auto-include-maybe (arg)
  "Automatically insert an appropriate C/C++ #include statement for the
prior symbol if we are not in a comment or a string."
  (interactive "p")
  ;; Are we in a comment or a string?
  (when (null (nth 8 (syntax-ppss)))
    (pd-cpp-add-using))
  (self-insert-command arg))

(defvar pd-cpp-auto-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?(]  'pd-cpp-auto-include-maybe)
      (define-key map [?<]  'pd-cpp-auto-include-maybe)
      (define-key map [?>]  'pd-cpp-auto-include-maybe)
      (define-key map [?&]  'pd-cpp-auto-include-maybe)
      (define-key map [?*]  'pd-cpp-auto-include-maybe)
      (define-key map [? ]  'pd-cpp-auto-include-maybe)
      (define-key map [?\;] 'pd-cpp-auto-include-maybe)
      map)
    "Keymap for auto-inserting #include statements in C++ mode. A limited
number of characters, such as space and '(<>&*;' trigger the automatic
insertion (the number of characters is kept as low as possible for
performance reasons)."
    )

(define-minor-mode pd-cpp-auto-mode
  "When enabled, automatically inserts a C/C++ #include statement
for the symbol just typed."
    nil "caut" pd-cpp-auto-keymap)


(pd-log-loading-complete)
(provide 'pd-cpp)
