var debug = true;

var emailSubject = "Auditions - Southampton University Chamber Choir";
var emailLines = [
    "Hello,",
    "Thanks for signing up for an audition at the bunfight yesterday!  " +
    "We had a really great turnout so are looking forward to an exciting weekend of auditions.  " +
    "Please check your date and time below and let me know if there are any mistakes.  " +
    "The auditions will be in the music building (building 2) in room 1083.  " +
    "We recommend arriving a little before your listed time just to make sure you find the " +
    "room alright, but please do arrive ready to sing as there aren't any rooms " +
    "available for warming up.  As mentioned at the bunfight, the audition will consist " +
    "of a short unaccompanied piece that we'd like you to prepare, a quick sight reading test, " +
    "and then some vocal and range tests.  All in all it should be about ten minutes per audition.",
    "Looking forward to seeing you all this weekend!"
];
var emailFooter = "\n\nKind regards,\n\nAlex Atack\nPresident\nSouthampton University Chamber Choir";

function main() {
    var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();

    var auditions = [];

    for (var i = 0; i < sheets.length; i++) {
        auditions = auditions.concat(getAuditions(sheets[i]));
    }

    sendEmails(auditions);
}

function sendEmails(auditions) {
    for (var i = 0; i < auditions.length; i++) {
        Logger.log("Sending emails (" + (i + 1) + "/" + auditions.length + ")");
        sendMail(
            auditions[i].email,
            emailSubject,
            join(emailLines) + formatAudition(auditions[i]) + emailFooter
        );
    }
}

function getAuditions(sheet) {
    var date = sheet.getName();
    var rows = sheet.getMaxRows();

    var times = sheet.getRange(1, 1, rows).getValues();
    var emails = sheet.getRange(1, 4, rows).getValues();
    var parts = sheet.getRange(1, 3, rows).getValues();
    var names = sheet.getRange(1, 2, rows).getValues();
    var auditions = [];

    extrapolateTimes(times);

    for (var i = 0; i < emails.length; i++) {
        if (couldBeEmail(emails[i][0])) {
            auditions.push({
                email: emails[i][0],
                name: names[i][0],
                part: parts[i][0].length > 0 ? parts[i][0] : "unknown",
                date: date,
                time: dateToTimeString(times[i][0])
            });
        }
    }

    return auditions;
}

function sendMail(email, subject, message) {
    if (debug) {
        Logger.log("Sent email to " + email + " titled '" + subject + "': " + message);
    } else {
        MailApp.sendEmail(email, subject, message);
    }
}

function couldBeEmail(candidate) {
    return candidate.indexOf("@") > 0;
}

function extrapolateTimes(times) {
    for (var i = 0; i < times.length; i++) {
        if (typeof times[i][0] != "object" && i > 0) {
            times[i] = times[i - 1];
        }
    }
}

function dateToTimeString(date) {
    minutes = date.getMinutes() == 0 ? "00" : date.getMinutes();
    return date.getHours() + ":" + minutes;
}

function formatAudition(audition) {
    return (
        "Name: " + audition.name +
        "\nPart: " + audition.part +
        "\nDate: " + audition.date +
        "\nTime: " + audition.time
    );
}

function join(lines) {
    var output = "";
    for (var i = 0; i < lines.length; i++) {
        output += lines[i];
        output += "\n\n";
    }
    return output;
}
