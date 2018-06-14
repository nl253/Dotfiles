if executable('go')
    " let s:gofmt_rules = [
                " \ "-r='buffer -> buf'", 
                " \ "-r='length -> len'",
                " \ "-r='message -> msg'",
                " \ "-r='job -> j'",
                " \ "-r='messages -> msgs'",
                " \ ]
    let s:gofmt_rules = []
    exe 'setl formatprg='.escape(join(['gofmt', '-s', join(s:gofmt_rules, ' ')], ' '), ' ')
    exe 'setl makeprg='.escape(join(['go', 'run', '%'], ' '), ' ')
endif

setl foldmarker={,} foldmethod=marker noexpandtab shiftwidth=2 tabstop=2
