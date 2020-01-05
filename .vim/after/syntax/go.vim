sy keyword goUnderscore _

let s:between_op = escape(join([
            \ ':=', 
            \ '=', 
            \ '==', 
            \ '>=', 
            \ '<=', 
            \ '<', 
            \ '>', 
            \ '/', 
            \ '%', 
            \ '+', 
            \ '-', 
            \ '-=', 
            \ '+=', 
            \ '*=', 
            \ '/=', 
            \ '\|\|', 
            \ '!='
            \ ], '|'), '=+><*%!')
exe 'sy match goOperatorBetween "\v ('.s:between_op.') "'

let s:right_op = escape(join([
            \ '&', 
            \ '*', 
            \ '*&', 
            \ '!', 
            \ '.{3}'
            \ ], '|'), '.&*')
exe 'sy match goOperatorRight "\v('.s:right_op.')<"'

let s:left_op = escape(join([
            \ '++', 
            \ '--',
            \ ',',
            \ ], '|'), '+')
exe 'sy match goOperatorLeft "\v>('.s:left_op.')"'

let s:sticky_op = escape(join([
            \ '%', 
            \ '-'
            \ ], '|'), '%')
exe 'sy match goOperatorSticky "\v>('.s:sticky_op.')<"'

let s:delims = escape(join([
            \ '{', 
            \ '}', 
            \ ';', 
            \ '[', 
            \ ']'
            \ ], '|'), '][}{')
exe 'sy match goDelim "\v'.s:delims.'"'

let s:std_pkgs = join([
            \ 'adler32',  
            \ 'archive',  
            \ 'ascii85',  
            \ 'asn1',  
            \ 'ast',  
            \ 'atomic',  
            \ 'base32',  
            \ 'base64',  
            \ 'bigbits',  
            \ 'binary',  
            \ 'bufio',  
            \ 'build',  
            \ 'builtin',  
            \ 'bytes',  
            \ 'bzip2',  
            \ 'cgi',  
            \ 'cgo',  
            \ 'cipher',  
            \ 'cmplx',  
            \ 'color',  
            \ 'compress',  
            \ 'container',  
            \ 'context',  
            \ 'cookiejar',  
            \ 'crc32',  
            \ 'crc64',  
            \ 'crypto',  
            \ 'csv',  
            \ 'database',  
            \ 'debug',  
            \ 'draw',  
            \ 'driver',  
            \ 'dwarf',  
            \ 'elf',  
            \ 'encoding',  
            \ 'errors',  
            \ 'exec',  
            \ 'expvar',  
            \ 'fcgi',  
            \ 'filepath',  
            \ 'flag',  
            \ 'flate',  
            \ 'fmt', 
            \ 'fmt',  
            \ 'fnv',  
            \ 'format',  
            \ 'gif',  
            \ 'go',  
            \ 'gob',  
            \ 'gosym',  
            \ 'gzip',  
            \ 'hash',  
            \ 'heap',  
            \ 'hex',  
            \ 'html',  
            \ 'http', 
            \ 'http',  
            \ 'httptest',  
            \ 'httptrace',  
            \ 'httputil',  
            \ 'image',  
            \ 'importer', 
            \ 'index',  
            \ 'io',  
            \ 'iotest',  
            \ 'ioutil',
            \ 'ioutil',  
            \ 'jpeg',  
            \ 'json',  
            \ 'jsonrpc',  
            \ 'list',  
            \ 'log', 
            \ 'log',  
            \ 'lzw',  
            \ 'macho',  
            \ 'mail',  
            \ 'math',  
            \ 'md5',  
            \ 'mime',  
            \ 'msan',  
            \ 'multipart',  
            \ 'net',  
            \ 'os',  
            \ 'palette',  
            \ 'parse',  
            \ 'parser',  
            \ 'path',  
            \ 'pe',  
            \ 'pem',  
            \ 'pkix',  
            \ 'plan9obj',  
            \ 'plugin',  
            \ 'png',  
            \ 'pprof',  
            \ 'printer',  
            \ 'quick',  
            \ 'quotedprintable',  
            \ 'race',  
            \ 'rand',  
            \ 'reflect',  
            \ 'regexp',  
            \ 'ring',  
            \ 'rpc',  
            \ 'rsa',  
            \ 'runtime',  
            \ 'scanner',  
            \ 'sha1',  
            \ 'sha256',  
            \ 'sha512',  
            \ 'signal',  
            \ 'smtp',  
            \ 'sort',  
            \ 'sql',  
            \ 'strconv',  
            \ 'strings',  
            \ 'subtle',  
            \ 'suffixarray',  
            \ 'sync',  
            \ 'syntax',  
            \ 'syscall',  
            \ 'syslog',  
            \ 'tabwriter',  
            \ 'tar',  
            \ 'template',  
            \ 'testing',  
            \ 'text',  
            \ 'textproto',  
            \ 'time',  
            \ 'tls',  
            \ 'token',  
            \ 'trace',  
            \ 'types',  
            \ 'unicode',  
            \ 'unsafe',
            \ 'url',  
            \ 'user',  
            \ 'utf16',  
            \ 'utf8',  
            \ 'x509',  
            \ 'xml',  
            \ 'zip',  
            \ 'zlib',  
            \ 'doc', 
            \ ], '|')
exe 'sy match goStdPackage "\v<('.s:std_pkgs.')\.@=>"'

sy match goFunction "\v([A-Za-z_]+[_a-z0-9A-Z]*)+\(@="
" sy match goFunction "\v\)"
sy keyword goSwitch case default

hi link goUnderscore      Operator
hi link goOperator        Operator
hi link goOperatorBetween Operator
hi link goOperatorRight   Operator
hi link goOperatorLeft    Operator
hi link goOperatorSticky  Operator
hi link goOperator        Operator
hi link goDelim           Delimiter
hi link goSwitch          Conditional
hi link goStdPackage      Include
