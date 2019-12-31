(require 'org-zettelkasten)

(defmacro org-zk-test (desc input output output-error &rest body)
  (let* ((base (file-name-directory (buffer-file-name)))
         (input (expand-file-name input base))
         (output (expand-file-name output base))
         (output-error (expand-file-name output-error base)))
    `(let (expected got)
       (with-temp-buffer
         (insert-file-contents ,output)
         (setq expected (buffer-string)))
       (with-current-buffer (find-file-noselect ,output-error)
         (erase-buffer)
         (insert-file-contents ,input)
         (org-mode)
         ,@body
         (setq got (buffer-string))
         (save-buffer))
       (if (string= expected got)
           (message "Test %s passed" ,desc)
         (progn
           (error "Test %s failed" ,desc))))))

(org-zk-test
 "Adding links"
 "test/fixtures/links.org"
 "test/fixtures/links_expected.org"
 "test/fixtures/links_got.org"
 (org-zk-add-link "friend" "foo.org")
 (org-zk-add-link "parent" "bar.org")
 (org-zk-add-link "child" "baz.org"))

(org-zk-test
 "Changing link types"
 "test/fixtures/link-type.org"
 "test/fixtures/link-type_expected.org"
 "test/fixtures/link-type_got.org"
 (org-zk-change-link-type "parent" "foo.org")
 (org-zk-change-link-type "child" "bar.org")
 (org-zk-change-link-type "friend" "baz.org"))

(org-zk-test
 "Removing links"
 "test/fixtures/link-remove.org"
 "test/fixtures/link-remove_expected.org"
 "test/fixtures/link-remove_got.org"
 (org-zk-remove-link "foo.org")
 (org-zk-remove-link "baz.org"))

(org-zk-test
 "Removing inline links"
 "test/fixtures/inline-link.org"
 "test/fixtures/inline-link-remove_expected.org"
 "test/fixtures/inline-link-remove_got.org"
 (org-zk-remove-inline-link "foo.org"))

(org-zk-test
 "Updating inline links"
 "test/fixtures/inline-link.org"
 "test/fixtures/inline-link-update_expected.org"
 "test/fixtures/inline-link-update_got.org"
 (org-zk-update-link "foo.org" nil "FOO")
 (org-zk-update-link "bar.org" "baz.org" "BAZ")
 (org-zk-update-link "baz.org" "bazz.org" nil))

(org-zk-test
 "Renaming files"
 "test/fixtures/links.org"
 "test/fixtures/rename_expected.org"
 "test/fixtures/rename_got.org"
 (org-zk-rename "Foo Bar"))
