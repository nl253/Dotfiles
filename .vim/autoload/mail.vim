fu! mail#send()
	let l:url   = mail#get_details('url', 'smtps://smtp.gmail.com:465')
	let l:flags = filter(map(['ssl-reqd', 'insecure'], '"--".v:val'), 'mail#check_flag(v:val)')
	let l:from  = mail#get_details('from', 'norbertlogiewa96@gmail.com')
	let l:to    = mail#get_details('to', 'norbertlogiewa96@gmail.com')
	let l:mail  = mail#get_details('mail_file', expand('%:p'))
	let l:user  = mail#get_details('user', 'norbertlogiewa96@gmail.com')
	let l:pass  = inputsecret('password => ')
	let l:cmd   = mail#get_details('curl command', join(
                \ ['curl', '--url', l:url] + l:flags +
                \ ['--mail-from',   l:from,
                \  '--mail-rcpt',   l:to,
                \  '--upload-file', l:mail,
                \  '--user',        l:user.':'.l:pass] , ' '))
	echo system(l:cmd)
endf

fu! mail#check_flag(flag)
	return tolower(input('flag '.a:flag.' is required correct [Y/N]? ')) ==# 'y'
endf

fu! mail#get_details(what, default)
	if tolower(input(a:what.' is '.string(a:default).' correct [Y/N]? ')) ==# 'y'
		return a:default
	else
		return input('edit '.a:what.' => ', a:default)
	endif
endf
