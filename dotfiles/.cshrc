# Add these lines to your ~/.cshrc.mine file on the linux grace machines...
# don't worry too much about what they mean.

# Colors!
set     red="%{\033[1;31m%}"
set   green="%{\033[1;32m%}"
set  yellow="%{\033[1;33m%}"
set    blue="%{\033[0;34m%}"
set magenta="%{\033[1;35m%}"
set    cyan="%{\033[1;36m%}"
set   white="%{\033[0;37m%}"
set     end="%{\033[0m%}" # This is needed at the end... :(

# Setting the actual prompt.  I made two separate versions for you to try, pick
# whichever one you like better, and change the colors as you want.  Just don't
# mess with the ${end} guy in either line...  Comment out or delete the prompt you don't use.

#set prompt="[${green}%n${green}@%m ${white}%~ ${green}%%${end} ] "
set prompt="[${green}%n${green}@%m ${white}%~: ${end}] "

# Clean up after ourselves...
unset red green yellow blue magenta cyan yellow white end

if ( -f ~/.aliases) then
  source ~/.aliases
endif

umask 22

  set addsuffix autocorrect autoexpand autolist chase_symlinks
  set history = 500
  set noclobber filec nobeep
  set symlinks=chase
  set correct = cmd
  set fignore = (.aux .cp .dvi .elc .fn .o .orig .pg .toc .tp .vr .bak '~')
  set rmstar

  setenv PAGER less
  setenv LESS eMs
  setenv EDITOR gedit
  setenv VISUAL gedit
