(when window-system
  (require 'prelude-packages)
  (when (display-graphic-p)
    (require 'all-the-icons))
  (scroll-bar-mode -1)
  (setq prelude-minimalistic-ui t)
  (prelude-require-package 'solarized-theme)
  (setq prelude-theme 'solarized-light))
