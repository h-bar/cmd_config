$PWD/reconfig.sh

sh -c "$(wget https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh -O -)"
echo "source $PWD/config/zinit/zinit.zsh" >> $HOME/.zshrc