interface Iso8601
    exposes [
        strToMsSinceEpoch,
        strToComponents,
        utf8ToMsSinceEpoch,
        utf8ToComponents,
        componentsToMsSinceEpoch,
    ]
    imports []

Components : {
    year : U32,
    month : U8,
    day : U8,
    hour : U8,
    minute : U8,
    second : U8,
    millisecond : U8,
}

## Parses a [Str] as an ISO-8601 date into a number of milliseconds since the Epoch.
strToMsSinceEpoch : Str -> Result U64 [BadIso8601Date _]
strToMsSinceEpoch = \str -> utf8ToMsSinceEpoch (Str.toUtf8 str)

utf8ToMsSinceEpoch : List U8 -> Result U64 [BadIso8601Date _]
utf8ToMsSinceEpoch = \bytes ->
    utf8ToComponents bytes
    |> Result.try componentsToMsSinceEpoch

strToComponents : Str -> Result Components [BadIso8601Date _]
strToComponents = \str -> utf8ToComponents (Str.toUtf8 str)

utf8ToComponents : List U8 -> Result Components [BadIso8601Date _]
utf8ToComponents = \bytes ->
    (ymd, rest) <- chompYYYYMMDD bytes |> Result.try

    { month, day } <- validateMonthDay ymd |> Result.try

    if List.isEmpty rest then
        Err (BadIso8601Date (LeftoverBytes rest))
    else
        Ok {
            year: ymd.year,
            month,
            day,
            hour: 0,
            minute: 0,
            second: 0,
            millisecond: 0,
        }

validateMonthDay : YearMonthDay -> Result { month : U8, day : U8 } [BadIso8601Date [InvalidMonth U32, InvalidDay U32]]
validateMonthDay = \{ year, month, day } ->
    when month is
        1 ->
            # 31 days in January
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 1, day: Num.toU8 day }

        2 ->
            # 28 days in February unless it's a leap year; then 29)
            if (day > 29) || (day == 29 && !(isLeapYear year)) || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 2, day: Num.toU8 day }

        3 ->
            # 31 days in March
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 3, day: Num.toU8 day }

        4 ->
            # 30 days in April
            if day > 30 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 4, day: Num.toU8 day }

        5 ->
            # 31 days in May
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 5, day: Num.toU8 day }

        6 ->
            # 30 days in June
            if day > 30 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 6, day: Num.toU8 day }

        7 ->
            # 31 days in July
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 7, day: Num.toU8 day }

        8 ->
            # 31 days in August
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 8, day: Num.toU8 day }

        9 ->
            # 30 days in September
            if day > 30 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 9, day: Num.toU8 day }

        10 ->
            # 31 days in October
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 10, day: Num.toU8 day }

        11 ->
            # 30 days in November
            if day > 30 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 11, day: Num.toU8 day }

        12 ->
            # 31 days in December
            if day > 31 || day == 0 then
                parseErr (InvalidDay day)
            else
                Ok { month: 12, day: Num.toU8 day }

        _ ->
            parseErr (InvalidMonth month)

## Convenience for writing (Err (BadIso8601Date ___))
parseErr : err -> [Err [BadIso8601Date err]]
parseErr = \err -> Err (BadIso8601Date err)

# TODO Why can't the signature be something like this?
# msInMonth : Int m, Int b -> Result (Int *) [InvalidMonth (Int m), InvalidDay (Int d)]
msInMonth : YearMonthDay -> Result U64 [BadIso8601Date [InvalidMonth U32, InvalidDay U32]]
msInMonth = \{ month, day: dayInMonth, year } ->
    when month is
        1 ->
            # 31 days in January
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # Add 0 days when in the first month of the year
                Ok 0

        2 ->
            # 28 days in February unless it's a leap year; then 29)
            if (dayInMonth > 29) || (dayInMonth == 29 && !(isLeapYear year)) || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in January
                Ok (31 * 24 * 60 * 60 * 1000)

        3 ->
            # 31 days in March
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 28 days in February (leap years are handled elsewhere)
                Ok ((28 + 31) * 24 * 60 * 60 * 1000)

        4 ->
            # 30 days in April
            if dayInMonth > 30 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in March
                Ok ((31 + 28 + 31) * 24 * 60 * 60 * 1000)

        5 ->
            # 31 days in May
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 30 days in April
                Ok ((30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        6 ->
            # 30 days in June
            if dayInMonth > 30 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in May
                Ok ((31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        7 ->
            # 31 days in July
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 30 days in June
                Ok ((30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        8 ->
            # 31 days in August
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in July
                Ok ((31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        9 ->
            # 30 days in September
            if dayInMonth > 30 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in August
                Ok ((31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        10 ->
            # 31 days in October
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 30 days in September
                Ok ((30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        11 ->
            # 30 days in November
            if dayInMonth > 30 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 31 days in October
                Ok ((31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        12 ->
            # 31 days in December
            if dayInMonth > 31 || dayInMonth == 0 then
                parseErr (InvalidDay dayInMonth)
            else
                # 30 days in November
                Ok ((30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + 28 + 31) * 24 * 60 * 60 * 1000)

        _ ->
            parseErr (InvalidMonth month)

componentsToMsSinceEpoch : Components -> Result U64 [BadIso8601Date [InvalidMonth U32, InvalidDay U32]]
componentsToMsSinceEpoch = \{ year, month, day: dayInMonth, hour, minute, second, millisecond } ->
    when msInMonth { year, month: Num.toU32 month, day: Num.toU32 dayInMonth } is
        Ok monthMs ->
            days =
                if month < 3 || !(isLeapYear year) then
                    # If we're in January or February, it doesn't matter
                    # if we're in a leap year from a days-in-month perspective.
                    # Only possible impact of laep years in this scenario is
                    # if we received February 29, which is checked later.
                    # Also, this doesn't matter if we explicitly aren't
                    # in a leap year.
                    dayInMonth - 1
                else
                    # We're in a leap year in March-December, so add an extra
                    # day (for Feb 29) compared to what we'd usually do.
                    dayInMonth

            # one extra day for each leap year
            dayMs =
                leapYearsBefore year
                |> Num.subWrap (leapYearsBefore epochYear)
                |> Num.addWrap (Num.toU32 days)
                |> Num.mulWrap msPerDay
                |> Num.toU64

            yearMs =
                year
                |> Num.subWrap epochYear
                |> Num.mulWrap msPerYear
                |> Num.toU64

            utcOffsetMinutes = 0 # TODO parse this

            hourMs = Num.toU64 hour * 60 * 60 * 1000
            minuteMs = (Num.toU64 minute - utcOffsetMinutes) * 60 * 1000
            secondMs = Num.toU64 second * 1000

            Num.toU64 monthMs
            |> Num.addWrap yearMs
            |> Num.addWrap dayMs
            |> Num.addWrap hourMs
            |> Num.addWrap minuteMs
            |> Num.addWrap secondMs
            |> Num.addWrap (Num.toU64 millisecond)
            |> Ok

        Err err -> Err err

YearMonthDay : { year : U32, month : U32, day : U32 }

msPerYear : Int _ # TODO should be able to annotate this as Int *
msPerYear = 365 * 24 * 60 * 60 * 1000

msPerDay : Int _ # TODO should be able to annotate this as Int *
msPerDay = 24 * 60 * 60 * 1000

epochYear : Int _ # TODO should be able to annotate this as Int *
epochYear = 1970

chompYYYYMMDD : List U8 -> Result (YearMonthDay, List U8) _
chompYYYYMMDD = \bytes ->
    (yyyy, rest1) <- chomp4digits bytes |> try
    rest2 <- chompSymbol '-' rest1 |> try
    (mm, rest3) <- chomp2digits rest2 |> try
    rest4 <- chompSymbol '-' rest3 |> try
    (dd, rest5) <- chomp2digits rest4 |> try

    Ok ({ year: yyyy, month: mm, day: dd }, rest5)

# Parse a date
expect
    actual =
        "1234-56-78"
        |> Str.toUtf8
        |> chompYYYYMMDD

    actual == Ok ({ year: 1234, month: 56, day: 78 }, [])

try = \result, transform ->
    when result is
        Ok answer -> transform answer
        Err err -> Err (BadIso8601Date err)
## From <https://www.timeanddate.com/date/leapyear.html>
##
## In the Gregorian calendar three criteria must be taken into account to identify leap years:
##
## - The year can be evenly divided by 4;
## - If the year can be evenly divided by 100, it is NOT a leap year, unless;
## - The year is also evenly divisible by 400. Then it is a leap year.
##
## This means that in the Gregorian calendar, the years 2000 and 2400 are leap years, while 1800, 1900, 2100, 2200, 2300 and 2500 are NOT leap years.
isLeapYear : Int _ -> Bool # TODO I should be able to annotate this as (Int *) but it gives an error!
isLeapYear = \year ->
    (year |> Num.rem 4) == 0 && (((year |> Num.rem 100) != 0) || (year |> Num.rem 400 == 0))

leapYearsBefore : Int a -> Int a
leapYearsBefore = \y1 ->
    y = y1 - 1

    (y // 4) - (y // 100) + (y // 400)

# state = List.walkUntil bytes emptyState walkUtf8

# when state.err is
#     None -> Ok state.msSinceEpoch
#     Err err -> Err (BadIso8601Date err)

# emptyState : State
# emptyState = {
#     err: None,
#     msSinceEpoch: 0,
#     inProgress: YYYY
#     wipNumber: 0,
# }

# State : {
#     err : [None, Err BadIso8601Date],
#     msSinceEpoch : U64,
#     # When we're parsing a number, we keep going until we hit something that isn't a number.
#     # Then we incorporate it into msSinceEpoch based on what the number meant (what was in progress).
#     inProgress : InProgress
#     wipNumber : 0,
# }

# walkUtf8 : State, U8 -> [Continue State, Break State]
# walkUtf8 = \state, byte ->
#     when parseDigit byte is
#         Ok digit ->
#             when Num.mulChecked state.msSinceEpoch 10 is
#                 Ok ms ->
#                     { state &
#                         msSinceEpoch: Num.addWrap ms digit,
#                     }
#                     |> Continue

#                 Err Overflow ->
#                     { state &
#                         err: Err U64Overflow
#                     }
#                     |> Break

#         Err NonDigitByte -> Break state

# parseDigit : U8 -> Result U8 [NonDigitByte]
# parseDigit = \byte ->
#     if byte >= 48 && byte <= 57 then
#         Ok (Num.subWrap byte 48)
#     else
#         Err NonDigitByte

chomp4digits : List U8 -> Result (U32, List U8) [NonDigitByte U8, Eof]
chomp4digits = \bytes ->
    (digit1, rest1) <- chompDigit bytes |> Result.try
    (digit2, rest2) <- chompDigit rest1 |> Result.try
    (digit3, rest3) <- chompDigit rest2 |> Result.try
    (digit4, final) <- chompDigit rest3 |> Result.try

    answer =
        (digit1 |> Num.mulWrap 1000)
        |> Num.addWrap (digit2 |> Num.mulWrap 100)
        |> Num.addWrap (digit3 |> Num.mulWrap 10)
        |> Num.addWrap digit4

    Ok (answer, final)

chomp2digits : List U8 -> Result (U32, List U8) [NonDigitByte U8, Eof]
chomp2digits = \bytes ->
    (digit1, rest1) <- chompDigit bytes |> Result.try
    (digit2, final) <- chompDigit rest1 |> Result.try

    answer =
        (digit1 |> Num.mulWrap 10)
        |> Num.addWrap digit2

    Ok (answer, final)

# Parse YYYY
expect
    actual =
        "1234"
        |> Str.toUtf8
        |> chomp4digits

    actual == Ok (1234, [])

# Parse MM
expect
    actual =
        "42"
        |> Str.toUtf8
        |> chomp2digits

    actual == Ok (42, [])

chompSymbol : U8, List U8 -> Result (List U8) [Unexpected { expected : U8, actual : U8 }, Eof]
chompSymbol = \symbol, bytes ->
    when List.first bytes is
        Ok byte ->
            if byte == symbol then
                # TODO have a List.pop (or something) which returns (first, rest)
                Ok (List.takeLast bytes (List.len bytes |> Num.subWrap 1))
            else
                Err (Unexpected { expected: symbol, actual: byte })

        Err ListWasEmpty -> Err Eof

chompDigit : List U8 -> Result (U32, List U8) [NonDigitByte U8, Eof]
chompDigit = \bytes ->
    when List.first bytes is
        Ok byte ->
            if byte >= 48 && byte <= 57 then
                digit = Num.toU32 (Num.subWrap byte 48)
                # TODO have a List.pop (or something) which returns (first, rest)
                rest = List.takeLast bytes (List.len bytes |> Num.subWrap 1)

                Ok (digit, rest)
            else
                Err (NonDigitByte byte)

        Err ListWasEmpty -> Err Eof
