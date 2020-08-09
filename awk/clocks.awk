BEGIN {
    todo = none;
    priority = "#B";
    hl = none;
    OFS = "\t";
}

BEGINFILE {
    title = FILENAME;
}

match($0, /^#\+TITLE:[ \t]+(.*)/, a) {
    title = a[1];
    next;
}

# TODO: Parse IDs

# All kinds of headings
# TODO: I can't get `org-complex-heading-regexp` to work
# as a workaround to ignore tags, assume headlines never contain three consecutive spaces
match($0, /^(\*+)[ \t]+(CANCELLED|DELEGATED|DONE|NEXT|PLANNING|SOMEDAY|TODO|WAITING)?([ \t]+(\[#.\]))?([\t ]+([^(   )]*))/, a) {
    hl = a[5];
    next;
}

match($0, /^[ \t]*CLOCK:[ \t]+(\[[^\]]*\])(--(\[[^\]]*\]))?/, a) {
    beg = a[1];
    end = a[3];
    print FILENAME, title, hl, beg, end
    next;
}
