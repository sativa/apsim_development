//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
// date format functions (based on http://www.mattkruse.com/) // 
function LZ(x) 
{
	return(x<0||x>9?"":"0")+x
}
function _isInteger(val) 
{
	var digits="1234567890";
	for (var i=0; i < val.length; i++) 
	{
		if (digits.indexOf(val.charAt(i))==-1)
			return false;
	}	
	return true;
}
function _getInt(str,i,minlength,maxlength) 
{
	var token = null; 
	for (var x=maxlength; x >= minlength; x--) 
	{
		token=str.substring(i,i+x);
		if (token.length < minlength)
			return null;
		if (_isInteger(token))
			return token; 
	}
	return null;
}
function isDate(val,format) 
{
	var date= getDateFromFormat(val,format);
	if(date==0)
		return false;
		
	return true;
}
function formatDate(date,format) 
{
	format=format+"";
	var result="";
	var i_format=0;
	var c="";
	var token="";
	var y=date.getYear()+"";
	var M=date.getMonth()+1;
	var d=date.getDate();
	var E=date.getDay();
	var H=date.getHours();
	var m=date.getMinutes();
	var s=date.getSeconds();
	var yyyy,yy,MMM,MM,dd,hh,h,mm,ss,ampm,HH,H,KK,K,kk,k;
	// Convert real date parts into formatted versions
	var value=new Object();
	if (y.length < 4) {y=""+(y-0+1900);}
	value["y"]=""+y;
	value["yyyy"]=y;
	value["yy"]=y.substring(2,4);
	value["M"]=M;
	value["MM"]=LZ(M);	
	value["d"]=d;
	value["dd"]=LZ(d);
	value["H"]=H;
	value["HH"]=LZ(H);
	if (H==0){value["h"]=12;}
	else if (H>12){value["h"]=H-12;}
	else {value["h"]=H;}
	value["hh"]=LZ(value["h"]);
	if (H>11){value["K"]=H-12;} else {value["K"]=H;}
	value["k"]=H+1;
	value["KK"]=LZ(value["K"]);
	value["kk"]=LZ(value["k"]);
	if (H > 11) { value["a"]="PM"; }
	else { value["a"]="AM"; }
	value["m"]=m;
	value["mm"]=LZ(m);
	value["s"]=s;
	value["ss"]=LZ(s);
	while (i_format < format.length) 
	{
		c=format.charAt(i_format);
		token="";
		while ((format.charAt(i_format)==c) && (i_format < format.length)) 
		{
			token += format.charAt(i_format++);
		}
		if (value[token] != null)
			result=result + value[token];
		else
			result=result + token;
	}
	return result;
}
function getDateFromFormat(val,format) 
{
	val=val+"";
	format=format+"";
	var i_val=0;
	var i_format=0;
	var c="";
	var token="";
	var token2="";
	var x,y;
	var now=new Date();
	var year=now.getYear();
	var month=now.getMonth()+1;
	var date=1;
	var hh=now.getHours();
	var mm=now.getMinutes();
	var ss=now.getSeconds();
	var ampm="";	
	while (i_format < format.length) {
		// Get next token from format string
		c=format.charAt(i_format);
		token="";
		while ((format.charAt(i_format)==c) && (i_format < format.length)) {
			token += format.charAt(i_format++);
			}
		// Extract contents of value based on format token
		if (token=="yyyy" || token=="yy" || token=="y") 
		{
			if (token=="yyyy") { x=4;y=4; }
			if (token=="yy")   { x=2;y=2; }
			if (token=="y")    { x=2;y=4; }
			year=_getInt(val,i_val,x,y);
			if (year==null) { return 0; }
			i_val += year.length;
			if (year.length==2) {
				if (year < 70) { year=1900+(year-0); }
				else { year=2000+(year-0); }
				}
		}		
		else if (token=="MM"||token=="M") {
			month=_getInt(val,i_val,token.length,2);
			if(month==null||(month<1)||(month>12)){return 0;}
			i_val+=month.length;}
		else if (token=="dd"||token=="d") {
			date=_getInt(val,i_val,token.length,2);
			if(date==null||(date<1)||(date>31)){return 0;}
			i_val+=date.length;}
		else if (token=="hh"||token=="h") {
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<1)||(hh>12)){return 0;}
			i_val+=hh.length;}
		else if (token=="HH"||token=="H") {
			hh=_getInt(val,i_val,token.length,2);
			if(hh==null||(hh<0)||(hh>23)){return 0;}
			i_val+=hh.length;}		
		else if (token=="mm"||token=="m") {
			mm=_getInt(val,i_val,token.length,2);
			if(mm==null||(mm<0)||(mm>59)){return 0;}
			i_val+=mm.length;}
		else if (token=="ss"||token=="s") {
			ss=_getInt(val,i_val,token.length,2);
			if(ss==null||(ss<0)||(ss>59)){return 0;}
			i_val+=ss.length;}
		else if (token=="a") {
			if (val.substring(i_val,i_val+2).toLowerCase()=="am") {ampm="AM";}
			else if (val.substring(i_val,i_val+2).toLowerCase()=="pm") {ampm="PM";}
			else {return 0;}
			i_val+=2;}
		else {
			if (val.substring(i_val,i_val+token.length)!=token) {return 0;}
			else {i_val+=token.length;}
			}
		}
	// If there are any trailing characters left in the value, it doesn't match
	if (i_val != val.length) { return 0; }
	// Is date valid for month?
	if (month==2) {
		// Check for leap year
		if ( ( (year%4==0) && (year%100 != 0) ) || (year%400==0) ) { // leap year
			if (date > 29){ return false; }
			}
		else { if (date > 28) { return false; } }
		}
	if ((month==4)||(month==6)||(month==9)||(month==11)) {
		if (date > 30) { return false; }
		}
	// Correct hours value
	if (hh<12 && ampm=="PM")
		hh=hh-0+12;
	else if (hh >11 && ampm=="AM")
		hh-=12;
	
	var newdate=new Date(year,month-1,date,hh,mm,ss);
	return newdate;
}
// GridEXCalendarCombo
function GridEXCalendarComboDropDown(calendarID, monthNames, firstDayWeek)
{	
	if(monthNames == null || firstDayWeek == null)
		throw Error("arguments of GridEXCalendarComboDropDown constructor are null or invalid");
		
	var id = calendarID;
	var button = document.getElementById(calendarID + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXCalendarDropDown '" + calendarID + "'"); 
		
	var textbox = document.getElementById(calendarID + "_textbox"); 
	if(textbox == null)
		throw Error("unable to find textbox for GridEXCalendarDropDown '" + calendarID + "'"); 
		
	var calendar = document.getElementById(id); 
	if(calendar == null)
		throw Error("calendar is null or invalid"); 
		
	var cellMonthCaption = null; 
	var daysTable = null;	
	var monthsScrollable = null; 
	var _divs = calendar.getElementsByTagName("DIV"); 
	cellMonthCaption = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[1]; 
	daysTable = _divs[1].getElementsByTagName("TABLE")[0]; 
	var owner = null; 	
	var currMonth = -1;
	var currDay = -1; 
	var currYear = -1; 		
	var firstDayWeek = firstDayWeek;
	var monthNames = monthNames;	
	var left = -1; 
	var top = -1; 
	var width = -1;
	var height = -1;	
	var monthDays = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);	
	var selectedDate = null; 	
	this.getDatePattern = getDatePattern; 
	this.getID = getID;
	this.getInnerTextBox = getInnerTextBox; 
	this.getSelectedDateString = getSelectedDateString; 
	this.getSelectedDate = getSelectedDate;	
	this.setLeft = setLeft;	
	this.setTop = setTop; 
	this.setHeight = setHeight;
	this.setWidth = setWidth; 
	this.setOwner = setOwner; 			
	this.setSelectedDate = setSelectedDate;
	this.setStyle = setStyle; 	
	this.Hide = Hide; 	
	this.Show = Show;	
	function initializeMonthDays()
	{
		var _isLeapYear = false; 
		if((currYear % 4) == 0)
		{
			if(((currYear % 100) != 0) || ((currYear % 400) == 0))
				_isLeapYear = true;
		}		
		if(_isLeapYear)
			monthDays[1] = 29;
		else
			monthDays[1] = 28; 
	}	
	function getDaysInMonth(month)
	{	
		if(month < 0 || month >= 12)
			throw Error("invalid month number");
	
		return monthDays[month]; 
	}			
	function getMonth() { return currMonth; }	
	function getMonthName(month)
	{
		if(month < 0 || month >= 12)
			throw Error("invalid month number"); 
			
		return monthNames[month]; 
	}	
	function getYear() { return currYear; }	
	function isSelectedDate(day, month, year)
	{
		if(selectedDate == null)
			return false;
			
		return (selectedDate.getDate() == day && selectedDate.getMonth() == month && selectedDate.getFullYear() == year); 						
	}	
	function isTodayDate(date, day, month, year)
	{		
		return (date.getDate() == day && date.getMonth() == month && date.getFullYear() == year); 
	}	
	function getID() { return id; }	
	function setOwner(value) { owner = value; }	
	function setSelectedDate(value)
	{
		if(value == null || value == "")
		{			
			selectedDate = null; 			
			var date = new Date(); 
			currDay = date.getDate(); 
			currMonth = date.getMonth(); 
			currYear = date.getFullYear(); 
		}
		else
		{		
			if(!isDate(value, getDatePattern()))
				throw Error("value argument is not a valid date");
				
			selectedDate = getDateFromFormat(value, getDatePattern()); 			
			updateTextBox(); 			
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 						
		}
	}	
	function getInnerTextBox() { return textbox; }	
	function getSelectedDate() { return selectedDate; }	
	function getSelectedDateString()
	{
		if(selectedDate != null)
			return formatDate(getSelectedDate(), getDatePattern()); 
		else
			return ""; 
	}	
	function getDatePattern()
	{
		var pattern = null; 
		if(owner != null)
			pattern = owner.getCell().getColumn().datePattern;
		if(pattern == null)
			return "dd/MM/yyyy"; 
		else
			return pattern;
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setStyle(style)
	{
		if(style != null)
		{
			textbox.style.fontFamily = style.getPropertyValue("font-family"); 
			textbox.style.fontSize = style.getPropertyValue("font-size"); 
			textbox.style.textAlign = style.getPropertyValue("text-align"); 
		}
	}	
	function setTop(value) { top = value; }	
	function setWidth(value) { width = value; }	
	function setHeight(value) { height = value; }
	function commitNewValue()	
	{
		if(isDate(textbox.value, getDatePattern()))
		{
			var date = getDateFromFormat(textbox.value, getDatePattern()); 
			if(date != getSelectedDate())
			{
				selectedDate = date; 				
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
					
				updateTextBox(); 
			}
		}
	}	
	function updateTextBox() { textbox.value = getSelectedDateString(); 	}		
	function updateDropDown()
	{
		if(textbox.value != getSelectedDateString())
		{			
			var date = null; 
			if(!isDate(textbox.value, getDatePattern()))
			{
				date = getSelectedDate(); 
				if(date == null)
					date = new Date(); 				
			}				
			else
				date = getDateFromFormat(textbox.value, getDatePattern());
				
			if(date != getSelectedDate())
			{
				selectedDate = date; 
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
			}			
			currDay = date.getDate(); 
			currMonth = date.getMonth();
			currYear = date.getFullYear(); 
		}	
		else if(selectedDate != null)
		{
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 			
		}			
		initializeMonthDays(); 		
		UpdateCalendar(true); 		
		var scrollheight = -1;
		var fixedtop = -1;
		scrollheight = window.innerHeight; 
		calendar.style.display = "block";
		calendar.style.visibility = "visible";
		if((top + height + 2) + calendar.offsetHeight > scrollheight)
			fixedtop = top - calendar.offsetHeight;
		if(fixedtop != -1)
			calendar.style.top = fixedtop + "px";
		else
			calendar.style.top = ((top + button.offsetHeight) + 1) + "px";
		var proposedLeft = (left + width) - calendar.offsetWidth;
		if(proposedLeft < 0)
		{
			try
			{
				proposedLeft = 0; 
				proposedLeft += getPixelWidth(document.defaultView.getComputedStyle(document.body, null).getPropertyValue("padding-left")); 
				proposedLeft += getPixelWidth(document.defaultView.getComputedStyle(document.body, null).getPropertyValue("margin-left")); 
			}
			catch(err)
			{ } 
		}		
		calendar.style.left = proposedLeft + "px"; 
	}			
	function UpdateCalendar(rebindYear)
	{	
		var date = new Date();		
		var cellFilled = 0; 		
		var firstDay = (new Date(currYear, currMonth, 1)).getDay();
		firstDay = ((firstDay - firstDayWeek) < 0) ? ((firstDay - firstDayWeek) + 7) :  firstDay - firstDayWeek;		
		var lastDay = getDaysInMonth(currMonth);
		var lastPreviousMonthDay = -1;
		var nextFirstMonthDay = 1; 		
		var previousMonth = -1;
		var previousYear = -1; 
		var nextMonth = -1;
		var nextYear = -1; 		
		if(firstDay != 0)
		{
			if(currMonth  == 0) // january - obtain december 
			{
				previousMonth = 11; 
				previousYear = getYear() - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth);
			}
			else
			{
				previousMonth = currMonth - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth); 
			}
		}		
		if(currMonth == 11) // december
		{
			nextYear = getYear() + 1; 
			nextMonth = 0; 
		}
		else
			nextMonth = currMonth + 1; 
	
		var dayCell = firstDay;		
		var day = 1;			
		var irow = 1; 
		var row = daysTable.rows[irow]; 		
		var cell = null; 		
		if(lastPreviousMonthDay != -1)
		{		
			var lastDayCell = dayCell-1;
			while(lastDayCell >= 0)
			{
				cell = row.cells[lastDayCell+1]; 
				cell.className = getID() + "_other_monthday";
				cell.innerHTML = lastPreviousMonthDay;
				cell.setAttribute("month", previousMonth); 				
				if(previousYear != -1)
					cell.setAttribute("year", previousYear);
				else
					cell.setAttribute("year", -1);
	
				lastDayCell--; 
				lastPreviousMonthDay--;
				cellFilled++; 
			}
		}		
		while(day <= lastDay)
		{
			if((dayCell != 0) && ((dayCell  % 7) == 0))
			{
				irow++;
				row = daysTable.rows[irow];
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 			
			if(isTodayDate(date, day, currMonth, currYear) && (selectedDate != null && !isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_today_notsel"; 				
			else if(isTodayDate(date, day, currMonth, currYear) && (selectedDate == null || isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_sel"; 
			else if(isSelectedDate(day, currMonth, currYear))
				cell.className = getID() + "_sel"; 
			else
				cell.className = getID() + "_monthday";
	
			cell.innerHTML = day;
			cell.setAttribute("month", getMonth()); 
			cell.setAttribute("year", -1); 
			day++; 
			dayCell++;
			cellFilled++; 
		}		
		while(cellFilled < 42)
		{			
			if((dayCell % 7) == 0)
			{
				irow++;
				row = daysTable.rows[irow]; 
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 
			cell.className = getID() + "_other_monthday"; 
			cell.innerHTML = nextFirstMonthDay; 
			cell.setAttribute("month", nextMonth); 
			if(nextYear != -1)
				cell.setAttribute("year", nextYear); 
			else
				cell.setAttribute("year", -1);
	
			dayCell++;
			cellFilled++;
			nextFirstMonthDay++; 
		}		
		cellMonthCaption.innerHTML = getMonthName(currMonth) + "&nbsp;" + currYear;
	}	
	function HideButton()
	{		
		button.style.visibility = "hidden"; 
	}	
	function HideCalendar()
	{
		calendar.style.visibility = "hidden"; 
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.setAttribute("status", 0);
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}
	function HideScrollableMonths()
	{
		monthsScrollable.style.visibility = "hidden"; 
	}
	function HideTextBox()
	{
		textbox.style.visibility = "hidden"; 
		textbox.value = ""; 
	}	
	function Hide()
	{
		HideCalendar(); 
		HideButton();
		HideTextBox(); 
	}
	function Show()
	{
		button.style.visibility = "visible"; 
		button.style.top = top + "px"; 
		button.style.left = (left + width - button.offsetWidth) + "px"; 
		button.style.height = height + "px";
		textbox.style.visibility = "visible"; 
		textbox.style.top = top + "px"; 
		textbox.style.left = left + "px";
		textbox.style.height = height + "px"; 
		textbox.style.width = (width - button.offsetWidth) + "px";
		textbox.focus(); 
	}	
	function button_onblur()
	{			
		if(button.style.visibility == "hidden")
			return; 
	
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
	
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		Hide(); 
		return true; 
	}	
	function button_onmousedown()
	{		
		if(calendar.style.visibility != "visible")
		{
			if(owner != null && owner.DropDown != null)
				owner.DropDown(); 
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			updateDropDown();
			if(calendar.focus != null)
				calendar.focus(); 			
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 0);
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			HideCalendar(); 
		}
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
	}	
	function button_onkeydown()
	{
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown();
	}
	function textbox_onblur()
	{			
		if(document.activeElement == button || button.contains(document.activeElement))		
			return true; 				
		if(document.activeElement == calendar || calendar.contains(document.activeElement))		
			return true;			
		if(owner != null && owner.Leaving != null)
			owner.Leaving();
			
		Hide();
		return true; 
	}
	function textbox_onchange()
	{		
		if(textbox.style.visibility == "visible")
			return; 
			
		commitNewValue(); 
	}	
	function textbox_onkeydown()
	{
		if(window.event.keyCode == 13)
		{		
			commitNewValue();			
			window.event.returnValue = false; 
			window.event.cancelBubble = true;					
		}
		else if(window.event.keyCode == 9)
			commitNewValue(); 
			
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
		
		return true; 
	}
	function textbox_onkeyup()
	{
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}	
	function calendar_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
		{
			HideCalendar(); 
			return true; 
		}
			
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving();
		else
			Hide(); 		
			
		return true; 
	}	
	function calendar_onkeydown()
	{
		if(window.event.keyCode == 27 || window.event.keyCode == 13)
		{
			if(owner != null && owner.Leaving != null)
				owner.Leaving(); 
				
			HideCalendar(); 
			textbox.focus(); 			
			if(window.event.keyCode == 13)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}
		else if(window.event.keyCode == 9)
		{
			Hide(); 
			if(owner != null && owner.KeyDown != null)
				owner.KeyDown(); 
		}
	}	
	function calendar_onselectstart()
	{
		window.event.cancelBubble = true;
		window.event.returnValue = false; 
		return true;
	}
	function day_onclick(e)
	{
		var element = e.srcElement; 
		if(element == null || element.tagName != "TD" || element.innerHTML == "")
			return; 
			
		var dayCell = element; 		
		var _day = element.innerHTML;
		var _month = element.getAttribute("month"); 
		var _year = element.getAttribute("year");		
		var _selectedDate = new Date((_year == -1) ? getYear() : _year, (_month == -1) ? getMonth() : _month, _day);
		if(_selectedDate != getSelectedDate())
		{
			selectedDate = _selectedDate; 
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			updateTextBox(); 
		}		
		HideCalendar(); 
		textbox.focus(); 
	}	
	var initializedMonthsScrollable = false;
	function RefreshMonthsScrollable()	
	{	
		var rowmonth = null; 			
		var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0]; 
		rowmonth = scrollableTable.rows[3];
		rowmonth.setAttribute("imonth", getMonth()); 
		rowmonth.setAttribute("iyear", getYear()); 
		rowmonth.cells[0].innerHTML = getMonthName(getMonth()) + "&nbsp;" + getYear(); 		
		rowmonth.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = rowmonth; 
		var icount = 2; 
		var imonth = getMonth(); 
		var iyear = getYear(); 		
		while(icount >= 0)
		{
			if(imonth - 1 < 0)
			{
				imonth = 11;
				iyear = iyear -1; 
			}
			else
				imonth = imonth - 1; 
		
			rowmonth = scrollableTable.rows[icount];
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 
			icount = icount - 1; 
		}		
		icount = 4; 
		imonth = getMonth(); 
		iyear = getYear(); 
		while(icount <= 6)
		{		
			if(imonth + 1 > 11)
			{
				imonth = 0; 
				iyear = iyear + 1; 
			}	
			else
				imonth = imonth + 1; 

			rowmonth = scrollableTable.rows[icount]; 
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 			
			icount = icount + 1; 
		}		
	}
	function getMonthRowFromEvent()
	{
		var monthrow = null; 
		if(window.event.srcElement != null && window.event.srcElement.tagName == "TR")
			monthrow = window.event.srcElement; 
		else if(window.event.srcElement != null && window.event.srcElement.tagName == "TD")
			monthrow = window.event.srcElement.parentElement; 

		if(monthrow == null)
			throw Error("unable to get monthrow generator"); 
			
		return monthrow; 		
	}
	function month_onmouseover()
	{						
		if(lastSelectedMonth != null && lastSelectedMonth.className != getID() + "_normalmonth")
			lastSelectedMonth.className = getID() + "_normalmonth"; 			

		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = monthrow; 		
	}
	function month_onmouseout()
	{		
		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_normalmonth"; 
	}
	var lastSelectedMonth = null; 
	function RefreshSelectedMonth(monthrow)
	{
		var imonth = parseInt(monthrow.getAttribute("imonth"), 10); 
		var iyear = parseInt(monthrow.getAttribute("iyear"), 10); 
		if(imonth != getMonth() || iyear != getYear())
		{		
			currMonth = imonth; 
			currYear = iyear; 
			initializeMonthDays();
			UpdateCalendar(true); 
		}
		HideScrollableMonths(); 		
	}
	function month_onclick()
	{
		var monthrow = getMonthRowFromEvent(); 
		RefreshSelectedMonth(monthrow); 
		lastSelectedMonth = null; 
	}
	function scrollablemonths_onkeydown()
	{		
		if(window.event.keyCode == 27)
			HideScrollableMonths(); 
	}	
	var lastClientY = null; 
	function scrollablemonths_onmousemove() {	lastClientY = window.event.clientY; }
	function scrollablemonths_onmouseup()
	{		
		if(intervalID != null)
		{
			window.clearInterval(intervalID); 
			if(window.event.srcElement != null && lastSelectedMonth != null)
			{
				if(window.event.srcElement == lastSelectedMonth || lastSelectedMonth.contains(window.event.srcElement))
				{					
					RefreshSelectedMonth(lastSelectedMonth); 
					lastSelectedMonth = null; 
				}
				else
					HideScrollableMonths(); 
			}
			else
				HideScrollableMonths();

			intervalID = null; 
		}
	}
	function scrollablemonths_onselectstart()
	{  }
	var intervalID = null; 
	function monthcaption_onmousedown()
	{
		if(!initializedMonthsScrollable)
		{
			monthsScrollable = document.getElementById(getID() + "_scrollablemonths"); 
			if(monthsScrollable == null)
				throw Error("unable to find scrollable months list");
				
			monthsScrollable.addEventListener("keydown", scrollablemonths_onkeydown, false); 
			monthsScrollable.addEventListener("selectstart", scrollablemonths_onselectstart, false); 			
			document.body.addEventListener("mousemove", scrollablemonths_onmousemove, false);
			document.body.addEventListener("mouseup", scrollablemonths_onmouseup, false);			
			var _scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0]; 
			for(var irow = 0; irow < _scrollableTable.rows.length; irow++)
			{				
				var _row = _scrollableTable.rows[irow];
				_row.addEventListener("click", month_onclick, false); 
				_row.addEventListener("mouseover", month_onmouseover, false); 
				_row.addEventListener("mouseout", month_onmouseout, false); 
			}			
			initializedMonthsScrollable = true; 
		}	
		if(intervalID == null)
			intervalID = window.setInterval(scrollMonths, 400);
		
		RefreshMonthsScrollable(); 
		monthsScrollable.style.left = getPixelLeft(cellMonthCaption) + "px";
		monthsScrollable.style.top = (getPixelTop(cellMonthCaption) - (monthsScrollable.offsetHeight / 2)) + "px"; 
		monthsScrollable.style.width = cellMonthCaption.offsetWidth + "px"; 
		monthsScrollable.style.visibility = "visible"; 
		window.event.returnValue = false; 
	}
	function scrollMonths()
	{					
		if(lastClientY == null)
			return; 
			
		var scrollDirection = 0; 		
		if(lastClientY < getPixelValue(monthsScrollable.style.top))
			scrollDirection = -1; 
		else if(lastClientY > (getPixelValue(monthsScrollable.style.top) + monthsScrollable.offsetHeight))
			scrollDirection = 1; 			
		if(scrollDirection != 0)
		{			
			var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0];
			var _row = null; 
			var imonth = -1;
			var iyear = -1; 
			for(var irow = 0; irow < scrollableTable.rows.length; irow++)
			{				
				_row = scrollableTable.rows[irow]; 
				imonth = parseInt(_row.getAttribute("imonth"), 10); 
				iyear = parseInt(_row.getAttribute("iyear"), 10);
				if(scrollDirection == -1)
				{
					if(imonth - 1 < 0)
					{
						imonth = 11; 
						iyear = iyear - 1; 
					}
					else
						imonth = imonth - 1; 
				}
				else
				{
					if(imonth + 1 > 11)
					{
						imonth = 0; 
						iyear = iyear + 1; 
					}
					else
						imonth = imonth + 1; 
				}
				_row.setAttribute("imonth", imonth); 
				_row.setAttribute("iyear", iyear); 
				_row.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 	
			}
		}
	}
	function nextmonth_onclick()
	{		
		if(currMonth == 11)	
		{
			currYear++;
			currMonth = 0;
		}
		else
			currMonth++;
		
		initializeMonthDays();
		UpdateCalendar(false); 
		window.event.returnValue = false,
		window.event.cancelBubble = true; 
	}	
	function previousmonth_onclick()
	{
		if(currMonth == 0)		
		{
			currYear = currYear - 1; 
			currMonth = 11; 
		}
		else
			currMonth--; 
		
		initializeMonthDays();
		UpdateCalendar(false);
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
	}	
	function today_onclick()
	{
		var _selectedDate = new Date();
		if(_selectedDate != getSelectedDate())
		{
			selectedDate = _selectedDate; 			
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			updateTextBox(); 
		}		
		HideCalendar(); 
		textbox.focus(); 
	}	
	function none_onclick()
	{
		selectedDate = null; 		
		if(owner != null && owner.ValueChanged != null)
			owner.ValueChanged(); 
			
		updateTextBox(); 			
		HideCalendar();
		//textbox.focus(); 
	}		
	calendar.addEventListener("blur", calendar_onblur, false); 
	calendar.addEventListener("keydown", calendar_onkeydown, false);	
	calendar.addEventListener("select", calendar_onselectstart, false) ;
	button.addEventListener("blur", button_onblur, false);
	button.addEventListener("mousedown", button_onmousedown, false);
	button.addEventListener("keydown", button_onkeydown, false);
	textbox.addEventListener("blur", textbox_onblur, false); 
	textbox.addEventListener("keydown", textbox_onkeydown, false); 	
	textbox.addEventListener("keyup", textbox_onkeyup, false);
	textbox.addEventListener("change", textbox_onchange, false);
	var cmd = null;
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[0];
	cmd.addEventListener("mousedown", previousmonth_onclick, false); 
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[2];
	cmd.addEventListener("mousedown", nextmonth_onclick, false);
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[1]; 
	cmd.addEventListener("mousedown", monthcaption_onmousedown, false);
	var row = null; 	
	var dayCells = -1; 	
	var dayRows = daysTable.rows.length; 
	for(var i = 1; i < dayRows - 1; i++)
	{
		row = daysTable.rows[i];		
		dayCells = row.cells.length - 1;
		for(var dayCell = 1; dayCell < dayCells; dayCell++)
			row.cells[dayCell].addEventListener("mousedown", day_onclick, false); 
	}		
	if(_divs.length == 3) 
	{
		var cmdTable = _divs[2].getElementsByTagName("TABLE")[0]; 
		var _cmdcells = cmdTable.getElementsByTagName("TD"); 
		for(var i = 0; i < _cmdcells.length; i++)
		{
			cmd = _cmdcells[i]; 
			if(cmd.getAttribute("type") == "1")
				cmd.addEventListener("mousedown", today_onclick, false); 
			else if(cmd.getAttribute("type") == "2")
				cmd.addEventListener("mousedown", none_onclick, false); 
		}
	}		
	var gridEXCalendarDropDown = this;	
	return this;
}
function GridEXCalendarDropDown(calendarID, monthNames, firstDayWeek)
{	
	if(monthNames == null || firstDayWeek == null)
		throw Error("arguments of GridEXCalendarDropDown constructor are null or invalid");
		
	var id = calendarID;	
	var button = document.getElementById(calendarID + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXCalendarDropDown '" + calendarID +"'"); 
		
	var calendar = document.getElementById(id); 
	if(calendar == null)
		throw Error("calendar is null or invalid"); 
		
	var cellMonthCaption = null; 
	var daysTable = null;		
	var _divs = calendar.getElementsByTagName("DIV"); 
	var _table = _divs[0].getElementsByTagName("TABLE")[0]; 	
	var _cells = _table.getElementsByTagName("TD"); 
	cellMonthCaption = _cells[1];
	daysTable = _divs[1].getElementsByTagName("TABLE")[0]; 	
	var monthsScrollable = null; 
	var owner = null; 	
	var currMonth = -1;
	var currDay = -1; 
	var currYear = -1; 		
	var firstDayWeek = firstDayWeek;
	var monthNames = monthNames;	
	var left = -1; 
	var top = -1; 
	var width = -1;
	var height = -1;	
	var monthDays = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);	
	var selectedDate = null; 		
	this.getDatePattern = getDatePattern; 
	this.getID = getID;
	this.getSelectedDate = getSelectedDate;
	this.getSelectedDateString = getSelectedDateString; 	
	this.setLeft = setLeft;	
	this.setTop = setTop; 
	this.setHeight = setHeight;
	this.setWidth = setWidth; 
	this.setOwner = setOwner; 			
	this.setSelectedDate = setSelectedDate;	
	this.Hide = Hide; 	
	this.Show = Show;	
	function initializeMonthDays()
	{
		var _isLeapYear = false; 
		if((currYear % 4) == 0)
		{
			if(((currYear % 100) != 0) || ((currYear % 400) == 0))
				_isLeapYear = true;
		}		
		if(_isLeapYear)
			monthDays[1] = 29;
		else
			monthDays[1] = 28; 
	}	
	function getDaysInMonth(month)
	{	
		if(month < 0 || month >= 12)
			throw Error("invalid month number");
	
		return monthDays[month]; 
	}			
	function getMonth() { return currMonth; }	
	function getMonthName(month)
	{
		if(month < 0 || month >= 12)
			throw Error("invalid month number"); 
			
		return monthNames[month]; 
	}	
	function getYear() { return currYear; }	
	function isSelectedDate(day, month, year)
	{
		if(selectedDate == null)
			return false;
			
		return (selectedDate.getDate() == day && selectedDate.getMonth() == month && selectedDate.getFullYear() == year); 						
	}	
	function isTodayDate(date, day, month, year) { return (date.getDate() == day && date.getMonth() == month && date.getFullYear() == year); }		
	function getID() { return id; }	
	function setOwner(value) { owner = value; }	
	function setSelectedDate(value)
	{
		if(value == null || value == "")
		{			
			selectedDate = null; 			
			var date = new Date(); 
			currDay = date.getDate(); 
			currMonth = date.getMonth(); 
			currYear = date.getFullYear(); 
		}
		else
		{		
			if(!isDate(value, getDatePattern()))
				throw Error("value argument is not a valid date");
				
			selectedDate = getDateFromFormat(value, getDatePattern()); 
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear(); 
		}
	}	
	function getSelectedDate() { return selectedDate; }	
	function getSelectedDateString()
	{
		if(selectedDate != null)
			return formatDate(getSelectedDate(), getDatePattern()); 
		else
			return ""; 
	}	
	function getDatePattern()
	{
		var pattern = null; 
		if(owner != null)
			pattern = owner.getCell().getColumn().datePattern;
		if(pattern == null)
			return "dd/MM/yyyy"; 
		else
			return pattern;
	}	
	function setLeft(value) { left = value; }	
	function setTop(value) { top = value;  }	
	function setWidth(value) { width = value; }	
	function setHeight(value) { height = value; }			
	function updateDropDown()
	{		
		if(selectedDate != null)
		{
			currDay = selectedDate.getDate(); 
			currMonth = selectedDate.getMonth();
			currYear = selectedDate.getFullYear();
		}	
		initializeMonthDays(); 		
		UpdateCalendar(true); 				
		var scrollheight = -1;
		var fixedtop = -1;
		scrollheight = window.innerHeight;
		calendar.style.display = "block"; 
		calendar.style.visibility = "visible";
		if((top + height + 2) + calendar.offsetHeight > scrollheight)
			fixedtop = top - calendar.offsetHeight;
		if(fixedtop != -1)
			calendar.style.top = fixedtop + "px";		
		else
			calendar.style.top = ((top + button.offsetHeight) + 1) + "px";
		var proposedLeft = (left + width) - calendar.offsetWidth;
		if(proposedLeft < 0)
		{
			try
			{
				proposedLeft = 0; 
				proposedLeft += getPixelWidth(document.defaultView.getComputedStyle(document.body, null).getPropertyValue("padding-left"));
				proposedLeft += getPixelWidth(document.defaultView.getComputedStyle(document.body, null).getPropertyValue("margin-left")); 
			}
			catch(err)
			{ } 
		}
		calendar.style.left = proposedLeft + "px";
	}			
	function UpdateCalendar(rebindYear)
	{	
		var date = new Date();		
		var cellFilled = 0; 		
		var firstDay = (new Date(currYear, currMonth, 1)).getDay();
		firstDay = ((firstDay - firstDayWeek) < 0) ? ((firstDay - firstDayWeek) + 7) :  firstDay - firstDayWeek;		
		var lastDay = getDaysInMonth(currMonth);
		var lastPreviousMonthDay = -1;
		var nextFirstMonthDay = 1; 		
		var previousMonth = -1;
		var previousYear = -1; 
		var nextMonth = -1;
		var nextYear = -1; 		
		if(firstDay != 0)
		{
			if(currMonth  == 0) // january - obtain december 
			{
				previousMonth = 11; 
				previousYear = getYear() - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth);
			}
			else
			{
				previousMonth = currMonth - 1;
				lastPreviousMonthDay = getDaysInMonth(previousMonth); 
			}
		}		
		if(currMonth == 11) // december
		{
			nextYear = getYear() + 1; 
			nextMonth = 0; 
		}
		else
			nextMonth = currMonth + 1; 
		
		var dayCell = firstDay;		
		var day = 1;			
		var irow = 1; 
		var row = daysTable.rows[irow]; 		
		var cell = null; 		
		if(lastPreviousMonthDay != -1)
		{		
			var lastDayCell = dayCell-1;
			while(lastDayCell >= 0)
			{
				cell = row.cells[lastDayCell+1]; 
				cell.className = getID() + "_other_monthday";
				cell.innerHTML = lastPreviousMonthDay;
				cell.setAttribute("month", previousMonth); 				
				if(previousYear != -1)
					cell.setAttribute("year", previousYear);
				else
					cell.setAttribute("year", -1);
				lastDayCell--; 
				lastPreviousMonthDay--;
				cellFilled++; 
			}
		}		
		while(day <= lastDay)
		{
			if((dayCell != 0) && ((dayCell  % 7) == 0))
			{
				irow++;
				row = daysTable.rows[irow];
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 			
			if(isTodayDate(date, day, currMonth, currYear) && (selectedDate != null && !isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_today_notsel"; 				
			else if(isTodayDate(date, day, currMonth, currYear) && (selectedDate == null || isSelectedDate(day, currMonth, currYear)))
				cell.className = getID() + "_sel"; 
			else if(isSelectedDate(day, currMonth, currYear))
				cell.className = getID() + "_sel"; 
			else
				cell.className = getID() + "_monthday";
				
			cell.innerHTML = day;
			cell.setAttribute("month", getMonth()); 
			cell.setAttribute("year", -1); 
			day++; 
			dayCell++;
			cellFilled++; 
		}		
		while(cellFilled < 42)
		{			
			if((dayCell % 7) == 0)
			{
				irow++;
				row = daysTable.rows[irow]; 
				dayCell = 0; 
			}			
			cell = row.cells[dayCell+1]; 
			cell.className = getID() + "_other_monthday"; 
			cell.innerHTML = nextFirstMonthDay; 
			cell.setAttribute("month", nextMonth); 
			if(nextYear != -1)
				cell.setAttribute("year", nextYear); 
			else
				cell.setAttribute("year", -1);
			dayCell++;
			cellFilled++;
			nextFirstMonthDay++; 
		}
		cellMonthCaption.innerHTML = getMonthName(currMonth) + "&nbsp;" + currYear;
	}	
	function HideButton() { button.style.visibility = "hidden"; }	
	function HideCalendar()
	{
		calendar.style.visibility = "hidden";
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.setAttribute("status", 0);
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}	
	function HideScrollableMonths() { monthsScrollable.style.visibility = "hidden"; }
	function Hide()
	{		
		HideCalendar(); 
		HideButton() ;
	}					
	function Show()
	{
		button.style.visibility = "visible"; 
		button.style.top = top + "px"; 
		button.style.left = (left + width - button.offsetWidth) + "px"; 
		button.style.height = height + "px"; 
		if(button.focus != null)
			button.focus(); 
	}	
	var initializedMonthsScrollable = false;
	function RefreshMonthsScrollable()	
	{	
		var rowmonth = null; 			
		var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0];
		rowmonth = scrollableTable.rows[3];
		rowmonth.setAttribute("imonth", getMonth()); 
		rowmonth.setAttribute("iyear", getYear()); 
		rowmonth.cells[0].innerHTML = getMonthName(getMonth()) + "&nbsp;" + getYear(); 		
		rowmonth.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = rowmonth; 
		var icount = 2; 
		var imonth = getMonth(); 
		var iyear = getYear(); 		
		while(icount >= 0)
		{
			if(imonth - 1 < 0)
			{
				imonth = 11;
				iyear = iyear -1; 
			}
			else
				imonth = imonth - 1; 
		
			rowmonth = scrollableTable.rows[icount];
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 
			icount = icount - 1; 
		}		
		icount = 4; 
		imonth = getMonth(); 
		iyear = getYear(); 
		while(icount <= 6)
		{		
			if(imonth + 1 > 11)
			{
				imonth = 0; 
				iyear = iyear + 1; 
			}	
			else
				imonth = imonth + 1; 
		
			rowmonth = scrollableTable.rows[icount]; 
			rowmonth.setAttribute("imonth", imonth); 
			rowmonth.setAttribute("iyear", iyear); 
			rowmonth.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 			
			icount = icount + 1; 
		}		
	}
	function getMonthRowFromEvent()
	{
		var monthrow = null; 
		if(window.event.srcElement != null && window.event.srcElement.tagName == "TR")
			monthrow = window.event.srcElement; 
		else if(window.event.srcElement != null && window.event.srcElement.tagName == "TD")
			monthrow = window.event.srcElement.parentElement; 

		if(monthrow == null)
			throw Error("unable to get monthrow generator"); 
			
		return monthrow; 		
	}
	function month_onmouseover()
	{				
		if(lastSelectedMonth != null && lastSelectedMonth.className != getID() + "_normalmonth")
			lastSelectedMonth.className = getID() + "_normalmonth"; 			

		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_selectedmonth"; 
		lastSelectedMonth = monthrow; 		
	}
	function month_onmouseout()
	{		
		var monthrow = getMonthRowFromEvent(); 		
		monthrow.className = getID() + "_normalmonth"; 
	}
	var lastSelectedMonth = null; 
	function RefreshSelectedMonth(monthrow)
	{
		var imonth = parseInt(monthrow.getAttribute("imonth"), 10); 
		var iyear = parseInt(monthrow.getAttribute("iyear"), 10); 
		if(imonth != getMonth() || iyear != getYear())
		{		
			currMonth = imonth; 
			currYear = iyear; 
			initializeMonthDays();
			UpdateCalendar(true); 
		}
		HideScrollableMonths(); 		
	}
	function month_onclick()
	{
		var monthrow = getMonthRowFromEvent(); 
		RefreshSelectedMonth(monthrow); 
		lastSelectedMonth = null; 
	}
	function scrollablemonths_onkeydown()
	{		
		if(window.event.keyCode == 27)
			HideScrollableMonths(); 
	}	
	var lastClientY = null; 
	function scrollablemonths_onmousemove()
	{	
		if(monthsScrollable.style.visibility == "hidden")
			return; 
	
		lastClientY = window.event.clientY; 
	}
	function scrollablemonths_onmouseup()
	{	
		if(monthsScrollable.style.visibility == "hidden")
			return; 
	
		if(intervalID != null)
		{
			window.clearInterval(intervalID); 
			if(window.event.srcElement != null && lastSelectedMonth != null)
			{
				if(window.event.srcElement == lastSelectedMonth || lastSelectedMonth.contains(window.event.srcElement))
				{
					RefreshSelectedMonth(lastSelectedMonth); 
					lastSelectedMonth = null; 
				}
				else
					HideScrollableMonths(); 
			}
			else
				HideScrollableMonths();

			intervalID = null; 
		}
	}
	function scrollablemonths_onselectstart()
	{	}
	var intervalID = null; 
	function monthcaption_onmousedown()
	{
		if(!initializedMonthsScrollable)
		{
			monthsScrollable = document.getElementById(getID() + "_scrollablemonths"); 
			if(monthsScrollable == null)
				throw Error("unable to find scrollable months list");
				
			monthsScrollable.addEventListener("keydown", scrollablemonths_onkeydown, false); 			
			monthsScrollable.addEventListener("select", scrollablemonths_onselectstart, false); 				
			var _monthsTable = monthsScrollable.getElementsByTagName("TABLE")[0]; 
			for(var irow = 0; irow < _monthsTable.rows.length; irow++)
			{								
				var _row = _monthsTable.rows[irow];				
				_row.addEventListener("click", month_onclick, false); 
				_row.addEventListener("mouseover", month_onmouseover, false); 
				_row.addEventListener("mouseout", month_onmouseout, false); 				
			}			
			document.body.addEventListener("mousemove", scrollablemonths_onmousemove, false);
			document.body.addEventListener("mouseup", scrollablemonths_onmouseup, false);			
			initializedMonthsScrollable = true; 
		}	
		if(intervalID == null)
			intervalID = window.setInterval(scrollMonths, 400);

		RefreshMonthsScrollable(); 
		monthsScrollable.style.left = getPixelLeft(cellMonthCaption) + "px";
		monthsScrollable.style.top = (getPixelTop(cellMonthCaption) - (monthsScrollable.offsetHeight / 2)) + "px"; 
		monthsScrollable.style.width = cellMonthCaption.offsetWidth + "px"; 
		monthsScrollable.style.visibility = "visible"; 	
		window.event.returnValue = false; 
	}	
	function scrollMonths()
	{				
		if(lastClientY == null)
			return; 
			
		var scrollDirection = 0; 
		if(lastClientY < getPixelValue(monthsScrollable.style.top))
			scrollDirection = -1; 
		else if(lastClientY > (getPixelValue(monthsScrollable.style.top) + monthsScrollable.offsetHeight))
			scrollDirection = 1; 			

		if(scrollDirection != 0)
		{			
			var scrollableTable = monthsScrollable.getElementsByTagName("TABLE")[0];
			var _row = null; 
			var imonth = -1;
			var iyear = -1; 
			for(var irow = 0; irow < scrollableTable.rows.length; irow++)
			{				
				_row = scrollableTable.rows[irow]; 
				imonth = parseInt(_row.getAttribute("imonth"), 10); 
				iyear = parseInt(_row.getAttribute("iyear"), 10);
				if(scrollDirection == -1)
				{
					if(imonth - 1 < 0)
					{
						imonth = 11; 
						iyear = iyear - 1; 
					}
					else
						imonth = imonth - 1; 
				}
				else
				{
					if(imonth + 1 > 11)
					{
						imonth = 0; 
						iyear = iyear + 1; 
					}
					else
						imonth = imonth + 1; 
				}
				_row.setAttribute("imonth", imonth); 
				_row.setAttribute("iyear", iyear); 
				_row.cells[0].innerHTML = getMonthName(imonth) + "&nbsp;" + iyear; 	
			}
		}
	}
	function button_onblur()
	{
		if(button.style.visibility != "visible")
			return;

		if(document.activeElement == button || button.contains(document.activeElement))
			return true;
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true;					
		
		Hide(); 
		return true; 
	}	
	function button_onkeydown()
	{		
		if(window.event.keyCode == 13)
		{	
			window.event.returnValue = false;
			window.event.cancelBubble = true; 
		}		
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
	}	
	function button_onmousedown()
	{
		if(calendar.style.visibility != "visible")
		{
			if(owner != null && owner.DropDown != null)
				owner.DropDown(); 
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			updateDropDown(); 
			if(calendar.focus != null)
				calendar.focus(); 			
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 0);
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			HideCalendar(); 
		}
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
	}
	function calendar_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == calendar || calendar.contains(document.activeElement))
			return true; 			
		
		if(owner != null && owner.Leaving != null)
			owner.Leaving();
		else			
			Hide();
			
		return true; 
	}	
	function calendar_onkeydown()
	{
		if(window.event.keyCode == 27 || window.event.keyCode == 13)
		{
			if(owner != null && owner.Leaving != null)
				owner.Leaving(); 
				
			HideCalendar(); 
			button.focus(); 			
			if(window.event.keyCode == 13)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}
		else if(window.event.keyCode == 9)
		{
			Hide(); 
			if(owner != null && owner.KeyDown != null)
				owner.KeyDown(); 
		}
	}	
	function day_onclick(e)
	{
		var element = e.srcElement; // window.event.srcElement; 		
		if(element == null || element.tagName != "TD" || element.innerHTML == "")
			return; 
			
		var dayCell = element; 		
		var _day = element.innerHTML;
		var _month = element.getAttribute("month"); 
		var _year = element.getAttribute("year");		
		var _selectedDate = new Date((_year == -1) ? getYear() : _year, (_month == -1) ? getMonth() : _month, _day);
		var previousDate = getSelectedDate();
		if(_selectedDate != previousDate)
		{
			selectedDate = _selectedDate; 
			if(owner != null && owner.ValueChanged != null)
			{
				var x = owner.ValueChanged(); 
				if(x != null && x == true)
					selectedDate = previousDate; 
			}
		}
		HideCalendar(); 
		if(button.onfocus != null)
			button.onfocus(); 
		
	}	
	function nextmonth_onclick()
	{
		if(currMonth == 11)	
		{
			currYear++;
			currMonth = 0;
		}
		else 
			currMonth++; 
		
		initializeMonthDays();
		UpdateCalendar(false);
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}	
	function previousmonth_onclick()
	{
		if(currMonth == 0)		
		{
			currYear = currYear - 1; 
			currMonth = 11; 
		}
		else
			currMonth--; 
		
		initializeMonthDays();
		UpdateCalendar(false);
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
	}	
	function today_onclick()
	{
		var _selectedDate = new Date();
		var previousDate = getSelectedDate(); 
		if(_selectedDate != previousDate)
		{
			selectedDate = _selectedDate; 			
			if(owner != null && owner.ValueChanged != null)
			{
				var x = owner.ValueChanged(); 
				if(x != null && x == true)
					selectedDate = previousDate; 
			}
		}		
		HideCalendar(); 
		button.focus(); 
	}	
	function none_onclick()
	{
		selectedDate = null;
		var previousDate = getSelectedDate();
		if(owner != null && owner.ValueChanged != null)
		{
			var x = owner.ValueChanged();
			if(x != null && x == true)
				selectedDate = previousDate;
		}
		HideCalendar();
		button.focus(); 
	}	
	calendar.addEventListener("blur", calendar_onblur, false); 
	calendar.addEventListener("keydown", calendar_onkeydown, false);	
	button.addEventListener("blur", button_onblur, false); 
	button.addEventListener("keydown", button_onkeydown, false); 
	button.addEventListener("mousedown", button_onmousedown, false); 	
	var cmd = null; 	
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[0]; 
	cmd.addEventListener("mousedown", previousmonth_onclick, false); 
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[2]; 
	cmd.addEventListener("mousedown", nextmonth_onclick, false); 	
	cmd = _divs[0].getElementsByTagName("TABLE")[0].getElementsByTagName("TD")[1]; 
	cmd.addEventListener("mousedown", monthcaption_onmousedown, false);	
	var row = null; 	
	var dayCells = -1; 	
	var dayRows = daysTable.rows.length; 
	for(var dayRow = 1; dayRow < dayRows - 1; dayRow++)
	{
		row = daysTable.rows[dayRow];		
		dayCells = row.cells.length - 1;
		for(var dayCell = 1; dayCell < dayCells; dayCell++)
			row.cells[dayCell].addEventListener("mousedown", day_onclick, false); 
	}		
	if(_divs.length == 3)
	{
		var cmdTable = _divs[2].getElementsByTagName("TABLE")[0]; 
		var _cmdcells = cmdTable.getElementsByTagName("TD"); 
		for(var cmdIndex = 0; cmdIndex < _cmdcells.length; cmdIndex++)
		{
			cmd = _cmdcells[cmdIndex]; 
			if(cmd.getAttribute("type") == "1")
				cmd.addEventListener("mousedown", today_onclick, false); 
			else if(cmd.getAttribute("type") == "2")
				cmd.addEventListener("mousedown", none_onclick, false); 
		}
	}		
	var gridEXCalendarDropDown = this;	
	return this; 
}