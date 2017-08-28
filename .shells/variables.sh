
# PATH
for i in ~/{.fzf,.pyenv,.local}/bin; do
	[[ -d $i ]] && export PATH=$i:$PATH
done

# PYENV (PATH needs to be set before this)
eval "$(pyenv virtualenv-init -)"
eval "$(pyenv init -)"
