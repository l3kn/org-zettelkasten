BEGINFILE {
    i = 0;
    has_title = 0;
    OFS="\t";
}

match($0, /^#\+TITLE:[ \t]+(.*)/, a) {
    print FILENAME, a[1];
    has_title = 1;
    next;
}

match($0, /^#\+ZK_ALIAS:[ \t]+(.*)/, a) {
    print FILENAME, a[1];
    has_title = 1;
    next;
}

{
    i += 1;
    if (i > 10) {
        nextfile;
    }
}

ENDFILE {
    if (!has_title) {
        print FILENAME, FILENAME;
    }
}
