
# make working directories

shell-init(){
  # make working directories
  for i in ~/Projects/{Go,Python,Rust,WebDev}; do
    [[ ! -e $i ]] && mkdir -p $i
  done
}
