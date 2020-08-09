BEGIN {
    in_review_data = 0;
    in_properties = 0;
}

/^[ \t]*:REVIEW_DATA:$/ {
    in_review_data = 1;
}

/^[ \t]*:PROPERTIES:$/ {
   in_properties = 1;
}

/^[ \t]*:END:$/ {
    in_review_data = 0;
    in_properties = 0;
}

/^[ \t]*:ID:[ \t]*([^ ]+)/ {
    # Remove all whitespace, then remove the ":ID:" part
    if (in_properties) {
        id = substr(trim($0), 5);
    }
}

# All kinds of headings
/^\*+[ \t]+.*$/ {
    heading = $0;
}

# Date, Name of day, optional time, optional repetition
match($0, /(<[0-9]{4}-[0-9]{2}-[0-9]{2} [a-zA-Z]{3}( [0-9]{2}:[0-9]{2})?( [\.]?\+[0-9]+[dwmy])?>)/, a) {
    if (in_review_data == 0) {
        print FILENAME, id, a[1]
    }
}

# Convert "<2019-11-30 Sat 18:28>" style dates to "2019-11-30T18:28"
# so they can be compared using string comparison
function convert_date(date) {
    gsub("[<>]", "", date);
    gsub(" (Mon|Tue|Wed|Thu|Fri|Sat|Sun) ", "T", date);
    return date;
}

function trim(str) {
    gsub(/[ \t]/, "", str);
    return str;
}

function trim_surrounding(str) {
    gsub(/^[ \t]*/, "", str);
    gsub(/[ \t]*$/, "", str);
    return str;
}
