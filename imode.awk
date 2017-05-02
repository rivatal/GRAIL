BEGIN{
    file = "grail.ml"

    while((getline < file) > 0){
	if(match($0, /(\(\*)(\s+let say)/)){
	    print gensub(/(\(\*)(\s+let say)/, "\\2", "g")
	}
	else if(match($0, /.*display l \*\)/))
	    print gensub(/(.*display l)( \*\))/, "\\1", "g")

	else if(match($0, /let compile/))
	    print "(* " $0
	else if(match($0, /compile\(\);/))
	    print $0 "*)"
	else
	    print $0
    }
    if(close(file))
	print file " failed to close"
}
