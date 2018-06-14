if exists('b:vim_saner_apache_loaded') | finish | endif

if !exists('apache_version')
	let apache_version = "2.0"
endif

let b:vim_saner_apache_loaded = 1
