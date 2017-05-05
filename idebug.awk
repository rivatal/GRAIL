BEGIN{
    file = "infer.ml"

    while((getline < file) > 0){
        if(ARGV[1] == "d" && match($0, /(\(\*)(\s+ignore\(print_string.*)(\*\))/))
            print gensub(/(\(\*)(\s+ignore\(print_string.*)(\*\))/, "\\2", "g")
            
        else
            print $0
    }
    if(close(file))
        print file " failed to close"
}