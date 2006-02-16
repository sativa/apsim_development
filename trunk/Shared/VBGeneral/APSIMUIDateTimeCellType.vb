Public Class APSIMUIDateTimeCellType
    Inherits FarPoint.Win.Spread.CellType.DateTimeCellType

    Public Sub DefaultFormat()
        ' Date/Time format settings
        Me.DateTimeFormat = FarPoint.Win.Spread.CellType.DateTimeFormat.UserDefined
        Me.DateSeparator = "-"
        Me.UserDefinedFormat = "dd" + Me.DateSeparator + "MMM"
        Me.Format("dd" + Me.DateSeparator + "MMM")

        ' Day/Month Short and Long Formats
        Me.DayNames = New String() {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}
        Me.MonthNames = New String() {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", ""}
        Me.ShortDayNames = New String() {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"}
        Me.ShortMonthNames = New String() {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", ""}

        ' Set Date Picker
        Me.DropDownButton = True
        Me.SetCalendarText("OK", "Cancel")

    End Sub

End Class
