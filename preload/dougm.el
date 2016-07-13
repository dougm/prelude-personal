(when window-system
  (require 'prelude-packages)
  (scroll-bar-mode -1)
  (prelude-require-package 'solarized-theme)
  (setq prelude-theme 'solarized-dark))
