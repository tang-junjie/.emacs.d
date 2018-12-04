;;; xah-defuns.el --- Xah Lee's Emacs Lisp Functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* ((ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ξfile-list))))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.

URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-10-04"
  (interactive)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (rename-file x (replace-regexp-in-string " " "_" x)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.

URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-10-04"
  (interactive)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (rename-file x (replace-regexp-in-string " " "-" x)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-insert-date ()
  "Insert current date and or time.
Insert date in this format: yyyy-mm-dd.
When called with `universal-argument', prompt for a format to use.
If there's text selection, delete it first.

Do not use this function in lisp code. Call `format-time-string' directly.

URL `http://ergoemacs.org/emacs/elisp_insert-date-time.html'
version 2016-10-11"
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (let ((-style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 2016-10-10 Monday"
                  "2 → 2016-10-10T19:39:47-07:00"
                  "3 → 2016-10-10 19:39:58-07:00"
                  "4 → Monday, October 10, 2016"
                  "5 → Mon, Oct 10, 2016"
                  "6 → October 10, 2016"
                  "7 → Oct 10, 2016"
                  )) 0 1))
           0
           )))
    (insert
     (cond
      ((= -style 0)
       (format-time-string "%Y-%m-%d") ; "2016-10-10"
       )
      ((= -style 1)
       (format-time-string "%Y-%m-%d %A") ; "2016-10-10 Monday"
       )
      ((= -style 2)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        ((lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z")))
       ;; "2016-10-10T19:02:23-07:00"
       )
      ((= -style 3)
       (concat
        (format-time-string "%Y-%m-%d %T")
        ((lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z")))
       ;; "2016-10-10 19:10:09-07:00"
       )
      ((= -style 4)
       (format-time-string "%A, %B %d, %Y")
       ;; "Monday, October 10, 2016"
       )
      ((= -style 5)
       (format-time-string "%a, %b %d, %Y")
       ;; "Mon, Oct 10, 2016"
       )
      ((= -style 6)
       (format-time-string "%B %d, %Y")
       ;; "October 10, 2016"
       )
      ((= -style 7)
       (format-time-string "%b %d, %Y")
       ;; "Oct 10, 2016"
       )
      (t
       (format-time-string "%Y-%m-%d"))))))

(defun xah-process-image (@file-list @args-str @new-name-suffix @new-name-file-suffix )
  "Wrapper to ImageMagick's “convert” shell command.
*file-list is a list of image file paths.
*args-str is argument string passed to ImageMagick's “convert” command.
*new-name-suffix is the string appended to file. e.g. “_new” gets you “…_new.jpg”
*new-name-file-suffix is the new file's file extension. e.g. “.png”

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2015-10-19"
  (require 'dired)
  (mapc
   (lambda ($f)
     (let ( $newName $cmdStr )
       (setq $newName
             (concat
              (file-name-sans-extension $f)
              @new-name-suffix
              @new-name-file-suffix))
       (while (file-exists-p $newName)
         (setq $newName
               (concat
                (file-name-sans-extension $newName)
                @new-name-suffix
                (file-name-extension $newName t))))
       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq $cmdStr
             (format
              "convert %s '%s' '%s'"
              @args-str
              (file-relative-name $f)
              (file-relative-name $newName)))
       (shell-command $cmdStr)))
   @file-list )
  (revert-buffer))

(defun xah-dired-scale-image (@file-list @scale-percentage @sharpen?)
  "Create a scaled version of images of marked files in dired.
The new names have “-s” appended before the file name extension.

If `universal-argument' is called first, output is PNG format. Else, JPG.

When called in lisp code,
 *file-list is a list.
 *scale-percentage is a integer.
 *sharpen? is true or false.

Requires ImageMagick unix shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList
           (read-from-minibuffer "Scale %:")
           (y-or-n-p "Sharpen"))))
  (let ( ($outputSuffix (if current-prefix-arg ".png" ".jpg" )))
    (xah-process-image
     @file-list
     (format "-scale %s%% -quality 85%% %s " @scale-percentage (if @sharpen? "-sharpen 1" "" ))
     "-s" $outputSuffix )))

(defun xah-image-autocrop ()
  "Create a new auto-cropped version of image.
If current buffer is jpg or png file, crop it.
If current buffer is dired, do the file under cursor or marked files.

The created file has “_crop638.” in the name, in the same dir.
It's in png or jpg, same as the original.

Requires ImageMagick shell command “convert”

If `universal-argument' is called first, output is PNG format. Else, JPG.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2017-08-27"
  (interactive)
  (let (
        ($bfName (buffer-file-name))
        $newName
        $cmdStr
        )
    (if (string-equal major-mode "dired-mode")
        (progn
          (let (($flist (dired-get-marked-files)))
            (mapc
             (lambda ($f)
               (setq $newName (concat (file-name-sans-extension $f) "_crop638." (file-name-extension $f)))
               (setq $cmdStr (format "convert -trim '%s' '%s'" (file-relative-name $f) (file-relative-name $newName)))
               (shell-command $cmdStr))
             $flist ))
          (revert-buffer))
      (progn
        (if $bfName
            (let (($ext (file-name-extension $bfName)))
              (if (and (not (string-equal $ext "jpg"))
                       (not (string-equal $ext "png")))
                  (user-error "not png or jpg at %s" $bfName)
                (progn
                  (setq $cmdStr
                        (format
                         "convert -trim '%s' '%s'"
                         $bfName
                         (concat (file-name-sans-extension $bfName) "_crop638." $ext)))
                  (shell-command  $cmdStr )
                  (message  $cmdStr))))
          (user-error "not img file or dired at %s" $bfName))))))

(defun xah-dired-2png (@file-list)
  "Create a png version of images of marked files in dired.
Requires ImageMagick shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList)))
  (xah-process-image @file-list "" "-2" ".png" ))
(provide 'xah-defuns)

(defun xah-dired-2jpg (@file-list)
  "Create a JPG version of images of marked files in dired.
Requires ImageMagick shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList)))
  (xah-process-image @file-list "-quality 90%" "-2" ".jpg" ))

(defun xah-dired-remove-all-metadata (@file-list)
  "Remove all metatata of buffer image file or marked files in dired.
 (typically image files)
URL `http://xahlee.info/img/metadata_in_image_files.html'
Requires exiftool shell command.

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (list
    (cond
     ((string-equal major-mode "dired-mode") (dired-get-marked-files))
     ((string-equal major-mode "image-mode") (list (buffer-file-name)))
     (t (list (read-from-minibuffer "file name:"))))))
  (if (y-or-n-p "Sure to remove all metadata?")
      (mapc
       (lambda ($f)
         (let ($cmdStr)
           (setq $cmdStr
                 (format "exiftool -all= -overwrite_original '%s'" (file-relative-name $f))) ; relative paths used to get around Windows/Cygwin path remapping problem
           (shell-command $cmdStr)))
       @file-list )
    nil
    ))

(defun xah-dired-show-metadata (@file-list)
  "Display metatata of buffer image file or marked files in dired.
 (typically image files)
URL `http://xahlee.info/img/metadata_in_image_files.html'
Requires exiftool shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (list
    (cond
     ((string-equal major-mode "dired-mode") (dired-get-marked-files))
     ((string-equal major-mode "image-mode") (list (buffer-file-name)))
     (t (list (read-from-minibuffer "file name:"))))))
  (mapc
   (lambda ($f)
     (shell-command
      (format "exiftool '%s'" (file-relative-name $f))
      ;; relative paths used to get around Windows/Cygwin path remapping problem
      ))
   @file-list ))

;;; xah-defuns.el ends here
