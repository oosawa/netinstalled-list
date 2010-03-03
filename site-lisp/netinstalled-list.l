;;
;;
;; �� netinstalled-list ��
;;  -- Netinstaller �ŃC���X�g�[�����ꂽ�p�b�P�[�W�̈ꗗ���쐬����
;;
;;
;; ++++++++++++++++++++++++++++
;; �����ӎ���
;;      ���ۏ؂ł��B
;;
;; ++++++++++++++++++++++++++++
;; ���K�v�Ȃ���
;;      Netinstaller
;;      �iHATTORI Masashi ����ɂ��B�j
;;
;; ++++++++++++++++++++++++++++
;; ���蓮�ł̃C���X�g�[�����@
;;   1. ���̃v���O������ ~/site-lisp/ �ɃR�s�[����B
;;   2. ~/.xyzzy �� ~/site-lisp/siteinit.l �Ɉȉ��̋L�q��ǉ�����
;;      (autoload 'make-netinstalled-list "netinstalled-list" t)
;;   3. xyzzy���ċN���isiteinit.l�ɋL�q�����ꍇ�̓_���v�t�@�C�����č쐬�j����B
;;
;; ++++++++++++++++++++++++++++
;; ���g����
;;
;;   1. M-x make-netinstalled-list ��Enter��������HTML�ŏo�͂��邩�ǂ����������̂ŁA�͂��܂��͂�������I������
;;
;; ++++++++++++++++++++++++++++
;; �����C�Z���X
;;      ���̃\�[�X�R�[�h�ɂ� MIT���C�Z���X���K�p����܂��B
;;     <http://www.opensource.org/licenses/mit-license.php>
;;
;; Copyright (c) 2010 oosawa
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;
;;
;; ++++++++++++++++++++++++++++
;; ������
;; 2010/01/25   ver.0.1	    ���J

(require "ni/setup")

(provide "netinstalled-list")
(in-package "netinst")

(defvar *ni-installed-list-html-hooks* nil)
(defvar *ni-installed-list-title* "Netinstaller Installed Packages List on xyzzy")
(defvar *ni-installed-list-description* "xyzzy �� NetInstaller �o�R�ŃC���X�g�[������Ă���p�b�P�[�W�̈ꗗ")
(defvar *ni-installed-list-title-format* "~%~%* ~A~%~%~%")
(defvar *ni-installed-list-html-title-format* "<h1>~A</h1>~%")
(defvar *ni-installed-list-html-encoding* *encoding-utf8*)
(defvar *ni-installed-list-xyzzy-uri* "http://www.jsdlab.co.jp/~kamei/")

(defun make-netinstalled-list ()
  (interactive)
  (let ((data (data-read-from-file *install-data-file*))
        (site-temp (data-read-from-file *install-site-file*))
        (htmlp (yes-or-no-p "HTML �Ń��X�g���쐬���܂����H"))
        (lists ""))
    (dolist (app data)
      (let* ((app-name (app-name app))
             (app-ver (app-version app))
             (app-site (app-site app))
             (app-autor (app-author app))
             (site-uri (get-netinstalled-site-uri app-site site-temp)))
        (if htmlp
            (when site-uri (setf app-site (format nil "<a href=\"~A\" title=\"~A\">~A</a>" site-uri app-site app-site))
              (setf lists (concat lists (format nil "<li>~A ver.~A �i~A�j</li>~%" app-name app-ver app-site))))
          (setf lists (concat lists (format nil "�E~A ver.~A �i~A�j~%" app-name app-ver app-site))))
        ))
      (let* ((buf (create-new-buffer *ni-installed-list-title*))
           (st (make-buffer-stream buf)))
      (switch-to-buffer buf)
      (if htmlp
          (let ((charset (cond ((equal *ni-installed-list-html-encoding* *encoding-utf8*) "UTF-8")
                               ((equal *ni-installed-list-html-encoding* *encoding-euc-jp*) "euc-jp")
                               ((equal *ni-installed-list-html-encoding* *encoding-jis*) "iso-2022-jp")
                               ((equal *ni-installed-list-html-encoding* *encoding-sjis*) "Shift_JIS")
                               nil)))
            (html-mode)
            (run-hooks *ni-installed-list-html-hooks*)
            (set-buffer-fileio-encoding *ni-installed-list-html-encoding*)
            (format st "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%")
            (format st "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ja\" lang=\"ja\">~%")
            (format st "<head>~%")
            (when charset
              (format st "  <meta http-equiv=\"Content-Txype\" content=\"text/html; charset=~A\" />~%" charset))
            (format st "  <title>~A</title>~%" *ni-installed-list-title*)
            (format st "</head>~%")
            (format st "<body>~%")
            (format st *ni-installed-list-html-title-format* *ni-installed-list-title*)
            (when *ni-installed-list-description*
              (format st "<p>~A</p>~%" *ni-installed-list-description*))
            (format st "<ul>~%~A</ul>~%" lists)
            (format st "<div style=\"text-align:right;\"><a href=\"~A\">xyzzy</a> ver.~A</div>~%" *ni-installed-list-xyzzy-uri* (software-version))
            (format st "<div style=\"text-align:right;\">Generated ~A</div>~%" (format-date-string "%a, %d %B %Y %H:%M %Z"))
            (format st "</body></html>~%~%")
            )
        (progn
          (text-mode)
          (format st *ni-installed-list-title-format* *ni-installed-list-title*)
          (when *ni-installed-list-description*
            (format st "~A~%" *ni-installed-list-description*))
          (format st "~%~A~%" lists)
          (format st "xyzzy ver.~A~%" (software-version))
          (format st "Generated ~A~%" (format-date-string "%y-%m-%d %H:%M%Z"))
          ))
        )))

(defun get-netinstalled-site-uri (site-name sites-temp)
  (dolist (site sites-temp)
    (handler-case
        (when (equalp site-name (site-name site))
          (return (site-url site))))
    nil))
