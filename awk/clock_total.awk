BEGIN {
    now = strftime("%FT%T", systime());
    day = time_days_ago(1);
    week = time_days_ago(7);
    month = time_days_ago(30);
    open = 0;
}

/^[ \t]*CLOCK:/ {
    split($0, tokens, /[\[\]]/);

    from = parse_date(tokens[2]);
    if (length(tokens) < 5) {
        to = now;
        open += 1;
    } else {
        to = parse_date(tokens[4]);
    }

    dur = duration(from, to);
    if (from > day) {
        print FILENAME;
        daily_total += dur;
    }
    if (from > week) {
        weekly_total += dur;
    }
    if (from > month) {
        monthly_total += dur;
    }
}

END {
    print "Clocks open: " open;
    print "Day:         " humanize_duration(daily_total);
    print "Week:        " humanize_duration(weekly_total);
    print "Month:       " humanize_duration(monthly_total);
}

function humanize_duration(secs) {
    seconds = secs % 60;

    mins = (secs - seconds) / 60;
    minutes = mins % 60;

    hours = (mins - minutes) / 60;

    return sprintf("%3d:%02d:%02d", hours, minutes, seconds);
}

function duration(from, to) {
    from = date_to_time(from);
    to = date_to_time(to);
    return to - from;
}

function date_to_time(date) {
    gsub(/[-T:]/, " ", date);
    return mktime(date);
}

function parse_date(date) {
    split(date, date_tokens, " ");
    return date_tokens[1] "T" date_tokens[3] ":00";
}

# Time n days before the current time
function time_days_ago(n) {
    return strftime("%FT%T", systime() - 24 * 60 * 60 * n);
}
