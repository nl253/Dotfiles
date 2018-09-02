fu! BTreeHeight() dict
    if self['data'] == v:null
        return 0
    else
        return 1 + max([self['left'] == v:null ? 0 : self['left'].height(), self['right'] == v:null ? 0 : self['right'].height()])
    endif
endf

fu! BTreeInsert(v) dict
	if self['data'] == v:null 
		let self['data'] = a:v
		let self['left'] = MakeBTree()
		let self['right'] = MakeBTree()
		return self
	elseif a:v < self['data']
		return self['left'].insert(a:v)
	else
		return self['right'].insert(a:v)
	endif
endf

fu! BTreeContains(v) dict
    if self['data'] == v:null
		return v:false
    elseif self['data'] == a:v 
		return v:true
	elseif a:v < self['data']
		return self['left'].contains(a:v)
	else
		return self['right'].contains(a:v)
	endif
endf

fu! MakeBTree(mempty)
	return { 'left':  a:mempty, 
           \ 'right': a:mempty, 
           \ 'data':  a:mempty, 
           \ 'height':   function('BTreeHeight'), 
           \ 'contains': function('BTreeContains'), 
           \ 'insert':   function('BTreeInsert')
           \ }
endf
