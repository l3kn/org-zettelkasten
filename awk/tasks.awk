BEGINFILE {
    in_properties = 0;
}

/^[ \t]*:PROPERTIES:$/ {
   in_properties = 1;
}

/^[ \t]*:END:$/ {
    if (in_properties) {
        in_properties = 0;
    }
}

/^[ \t]*:ID:[ \t]*([^ ]+)/ {
    # Remove all whitespace, then remove the ":ID:" part
    if (in_properties) {
        id = substr(trim($0), 5);
    }
}

/^[ \t]*:EFFORT:[ \t]*([^ ]+)/ {
    # Remove all whitespace, then remove the ":EFFORT:" part
    if (in_properties) {
        effort = substr(trim($0), 9);
    }
}

# All kinds of headings
match($0, /^\*+[ \t]+(TODO|NEXT|DONE|CANCELLED|WAITING)?([ \t]\[(#A|#B|#C)\])?[ \t]+.*$/, a) {
    print $0;
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
