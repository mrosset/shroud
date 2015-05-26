((scheme-mode
  .
  ((indent-tabs-mode . nil)
   (eval . (put 'call-with-pipe* 'scheme-indent-function 2))
   (eval . (put 'call-with-output-pipe* 'scheme-indent-function 1))
   (eval . (put 'call-with-input-pipe* 'scheme-indent-function 1))
   (eval . (put 'call-with-encrypted-output-file 'scheme-indent-function 2))
   (eval . (put 'call-with-decrypted-input-file 'scheme-indent-function 1)))))
