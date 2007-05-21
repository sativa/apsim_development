//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
function retrieveInputLimits(input)
{		
	var s0, s1;
	s0 = s1 = 0;
	if(input.selectionStart != null)
	{
		s0 = input.selectionStart; 
		s1 = input.selectionEnd;
		return [s0, s1]; 
	}				
	var s = document.selection.createRange(); 
	var r = s.duplicate(); 
	r.move("textedit", -1); 
	try
	{
		while(r.compareEndPoints("StartToStart", s) < 0)
		{
			if(s0++ > 1000)
				break;
					
			r.moveStart("character", 1); 
		}
	} catch(err) { }
	r = s.text;
	s1 = s0 + r.length;		
	return [s0,s1]; 
}
function MaskedChar()
{
	var required, literal, acceptsLetter, acceptsDigit, acceptsSpace, acceptsPlusMinus, anyCharacter, isLiteral;
	this.Required = required;
	this.Literal = literal;
	this.AcceptsLetter = acceptsLetter;
	this.AcceptsDigit = acceptsDigit;
	this.AcceptsSpace = acceptsSpace;
	this.AcceptsPlusMinus = acceptsPlusMinus;
	this.AnyCharacter = anyCharacter;
	this.IsLiteral = isLiteral; 
}
function MaskedEditBox(id, definition, events)
{
	var id = id;
	var events = events;	
	this.onblur = onblur; 
	this.onfocus = onfocus; 
	this.onkeydown = onkeydown;
	this.onkeypress = onkeypress; 
	this.Mask = definition[0]; 
	this.PromptChar = definition[1]; 
	this.CharacterCasing = definition[2]; 
	var text = definition[3]; 		
	this.Text = getText;  
	var includeLiterals = (definition[4] == 1) ?  true : false; 
	this.IsTextValid = getIsTextValid; 
	function onblur()
	{
		input.value = getTextMasked(false); 
		fireEvent("Blur", [maskedEditBox]); 
	}
	function onfocus()
	{
		if(input.createTextRange != null)
			textRange = input.createTextRange();
		input.value = getTextMasked(true);
		fireEvent("Focus", [maskedEditBox]); 
	}
	function validateMaskedText(allowEmpties)
	{
		if(isMasked)
		{
			for(var i=0;i<chars.length;i++)
			{
				if(!chars[i].IsLiteral)
				{
					if(chars[i].Required && chars[i].Literal != null && chars[i].Literal.length == 0)
						return false; 
					else if(chars[i].Literal != null && chars[i].Literal.length > 0 && !isValidChar(i, chars[i].Literal.substr(0,1), allowEmpties))
						return false; 
				}
			}
		}
		return true; 
	}
	function getIsTextValid()
	{
		if(this.Mask.length > 0)
		{
			if(!isMasked)
				return false; 
				
			var prevIncludeLiterals = includeLiterals; 
			includeLiterals = false; 
			var s = getText(); 
			includeLiterals = prevIncludeLiterals; 
			if(s.length == 0)
				return true; 
				
			return validateMaskedText(true); 
		}
		else
			return true; 
	}
	function getText()
	{
		if(isMasked)
		{
			var s = ""; 
			var empty = true; 
			for(var i=0;i<chars.length;i++)
			{
				if(!(chars[i].Literal == null || chars[i].Literal.length == 0))
				{
					if(includeLiterals || !chars[i].IsLiteral)
					{
						s += chars[i].Literal; 
						if(!chars[i].IsLiteral)
							empty = false; 
					}
				}
			}
			if(empty)
				return "";
			if(this.CharacterCasing == 1)
				s = s.toLowerCase(); 
			else if(this.CharacterCasing == 3)
				s =  s.toUpperCase();
				
			return s; 
		}
		else
			return input.value; 
	}
	function getNextChar(p)
	{
		p++;
		while(p < chars.length && chars[p].IsLiteral)
			p++;
			
		return p; 
	}
	function isValidChar(p, keyChar, allowEmpties)	
	{
		var code = -1; 
		if(keyChar == null)			
			code = getKeyCode(window.event, input); 
		else
			code = keyChar.charCodeAt(0);
	
		if(chars[p].AnyCharacter)
			return true;
		if(allowEmpties && code == 32)
			return true;
		if(chars[p].AcceptsDigit)
		{			
			if(code >= 48 && code <= 57)
				return true;
		}
		if(chars[p].AcceptsLetter)
		{
			try
			{
				var s = String.fromCharCode(code); 
				if(code > 255 || (s.toLowerCase() != s.toUpperCase()))
					return true;
				else
					return false;
			}
			catch(err)
			{  } 
		}		
		if(chars[p].AcceptsSpace)
		{
			if(code == 32)
				return true; 
		}
		if(chars[p].AcceptsPlusMinus)
		{
			try
			{
				var s = String.fromCharCode(code); 
				if(s == "+" || s == "-")
					return true; 
			}
			catch(err)
			{ } 
		}
		return false; 
	}
	function replaceChar(p)
	{
		if(isMasked)
		{
			if(p >= 0 && p < chars.length)
			{
				if(chars[p].IsLiteral)
					return replaceChar(getNextChar(p));
				else
				{
					if(isValidChar(p))
					{
						var code = getKeyCode(window.event, input);
						var s = String.fromCharCode(code); 
						if(chars[p].AcceptsLetter)
						{
							if(definition[2] == 1)
								s = s.toLowerCase();
							else if(definition[2] == 3)
								s = s.toUpperCase();
						}
						chars[p].Literal = s; 						
						return true;
					}
					else
						return false;
				}
			}
			else 
				return false;
		}
		else
			return false; 
	}
	function selectionCore(p,e)
	{
		if(input.selectionStart != null)
		{
			input.readOnly = true; 
			input.selectionStart = p; 
			input.selectionEnd =  p+0; 
			input.readOnly = false; 
			return; 
		}
		s1 -= p;		
		textRange.move("textedit", -1); 
		textRange.move("character", p);
		textRange.select();
	}
	function eraseChar(p)
	{
		if(isMasked)
		{
			if(p >= 0 && p < chars.length)
			{
				if(!chars[p].IsLiteral)
					chars[p].Literal = ""; 
			}
		}
	}
	function clearMask(initial,length)
	{		
		if(length <= 0)
			length = 1;
		for(var i = initial;i<initial+length;i++)
			eraseChar(i); 
	}
	function processKeyChar()
	{
		var p = s0; 
		if(p >= 0 && p < chars.length)
		{				
			if(s1 - p > 0)
			{
				clearMask(p, s1-p); 
				input.value = getTextMasked(true); 
				selectionCore(p, 0); 
			}
			if(chars[p].IsLiteral)
				p = getNextChar(p);
				
			if(replaceChar(p))
			{
				p = getNextChar(p); 
				input.value = getTextMasked(true);				
				selectionCore(p,0); 				
			}						
		}	
		window.event.cancelBubble = true;
		window.event.returnValue = false; 		
	}
	function onkeypress()
	{
		var cancel = fireEvent("KeyPress", [maskedEditBox]);
		if(cancel == null || !cancel)
		{
			if(isMasked)
				processKeyChar(); 
		}
	}
	var s0 = s1 = -1; 
	var textRange; 	
	function onkeydown()
	{
		var cancel = fireEvent("KeyDown", [maskedEditBox]); 
		if(cancel == null || !cancel)
		{
			if(isMasked)
			{
				var cancel = false;
				var il = retrieveInputLimits(input);
				s0 = il[0];
				s1 = il[1]; 
				if(window.event.keyCode == 8)
				{
					if(s1-s0 == 0)
					{
						var p = s0 - 1; 
						if(p >= 0)
						{
							eraseChar(p);
							input.value = getTextMasked(true);
							if(p > 0 && chars != null && chars.length > p)
							{
								var pi = p;
								while(chars[pi-1].IsLiteral)
								{
									pi -= 1;
									if(pi < 1)
										break; 
								}
								p = pi; 
							}
							selectionCore(p); 
						}
					}
					else
					{
						var p = s0; 
						clearMask(s0, s1-s0); 
						input.value = getTextMasked(true); 
						selectionCore(p); 
					}
					cancel = true;				
				}
				else if(window.event.keyCode == 46)
				{
					var p = s0;
					clearMask(s0, s1-s0);
					input.value = getTextMasked(true);
					selectionCore(p);
					cancel = true; 
				}
				if(cancel)
				{
					window.event.returnValue = false;
					window.event.cancelBubble = true; 
				}
			}
		}
	}
	function fireEvent(name, params)
	{
		return fireEventEx(events, name, params); 
	}
	var chars = null;
	var fillLeftToRight = true; 
	var isMasked = false; 	
	function onMaskChanged()
	{
		var mask = definition[0]; 
		if(mask.length > 0)
		{						
			var isLiteral = false; 
			fillLeftToRight = true;
			isMasked = true; 
			var count = 0; 
			for(var i=0;i<mask.length;i++)
			{
				var s = mask.substr(i,1);
				if(!isLiteral)
				{
					if(s== "\\")
						isLiteral=true;
					else if(s != "!")
						count++;
				}
				else
				{
					count++;
					isLiteral = false; 
				}
			}
			isLiteral = false; 
			chars = new Array(count);
			for(var i=0;i<chars.length;i++)
				chars[i] = new MaskedChar();
			var index = 0;
			for(var i=0;i<mask.length;i++)
			{
				var s = mask.substr(i,1); 
				if(!isLiteral)
				{
					if(s!="\\")
					{
						switch(s)
						{
							case "!":
								fillLeftToRight = false;
								continue;
							case "0":
							{
								chars[index].Required = true;
								chars[index].AcceptsDigit = true;
							} break; 								
							case "9":
							{
								chars[index].AcceptsDigit = true;
								chars[index].AcceptsPlusMinus = true;
								chars[index].AcceptsSpace = true;
							} break; 
							case "#":
							{
								chars[index].AcceptsDigit = true;
								chars[index].AcceptsPlusMinus = true;
								chars[index].AcceptsSpace = true;
							} break; 
							case "L":
							{
								chars[index].Required = true;
								chars[index].AcceptsLetter = true; 
							} break; 
							case "?":
								chars[index].AcceptsLetter = true;
							break; 
							case "A":
							{
								chars[index].Required = true; 
								chars[index].AcceptsDigit = true;
								chars[index].AcceptsLetter = true; 
							} break; 
							case "a":
							{
								chars[index].AcceptsDigit = true;
								chars[index].AcceptsLetter = true; 
							} break;
							case "&":
							{
								chars[index].Required = true;
								chars[index].AnyCharacter = true;
							} break;
							case "C":
								chars[index].AnyCharacter = true;
							break;
							case ".":
							{
								chars[index].IsLiteral = true;
								chars[index].Literal = definition[6];  
							} break;
							case ",":
							{
								chars[index].IsLiteral = true;
								chars[index].Literal = definition[7]; 
							} break; 
							case ":" : 
							{
								chars[index].IsLiteral = true;
								chars[index].Literal = definition[8]; 
							} break; 
							case "/":
							{
								chars[index].IsLiteral = true;
								chars[index].Literal = definition[9]; 
							} break;
							default:
							{
								chars[index].IsLiteral = true;
								chars[index].Literal = s; 
							} break; 							
						}
						index++; 
					}
					else
						isLiteral = true; 				
				}
				else
				{
					chars[index].IsLiteral = true;
					chars[index].Literal = s;
					index++;
					IsLiteral = false;
				}
			}
		}
		else
		{
			isMasked = false;
			chars = new Array(0); 
		}
	}
	function getTextMasked(useprompt)
	{
		if(isMasked)
		{			
			var j = input.value.length;
			if(j != chars.length)
			{
				while(j<chars.length)
				{
					eraseChar(j);
					j++;
				}
			}
			var prompt = definition[1]; 
			if(!useprompt)
				prompt = ' '; 
			var text = "";
			for(var i=0;i<chars.length;i++)
			{
				if(chars[i].Literal == null || chars[i].Literal.length == 0)
					text += prompt;
				else
					text += chars[i].Literal;
			}
			return text;
		}
		else
			return getText(); 
	}	
	function fillMask(text)
	{		
		if(isMasked)
		{
			if(text.length > chars.length)
				return false;				
			for(var i=0;i<text.length;i++)
			{
				var schar = text.charAt(i); 
				if(schar.length > 0)
				{
					var mchar = chars[i]; 
					if(!mchar.IsLiteral)
					{
						if((mchar.Required && schar.charCodeAt(0) != 32) || (mchar.AcceptsDigit && !isNaN(schar.charCodeAt(0))))
							mchar.Literal = schar; 
					}
				}
			}
		}
	}
	if(this.Mask.length > 0)
		onMaskChanged(); 
	if(text.length > 0 && definition[4] == 1)
		fillMask(text); 	
	var input = document.getElementById(id); 
	// handle events
	browser.handleEvent(input, "focus", function() { return maskededitbox_onfocus(id); } ); 
	browser.handleEvent(input, "blur", function() { return maskededitbox_onblur(id); } ); 
	browser.handleEvent(input, "keydown", function() { return maskededitbox_onkeydown(id); } ); 
	browser.handleEvent(input, "keypress", function() { return maskededitbox_onkeypress(id); } );
	fireEvent("Load", [this]); 
	var maskedEditBox = this; 
	return this; 
}
function NumericEditBox(id, definition, events)
{
	var events = events; 
	var id = id; 			
	var keyDown = null; 
	var lostFocus = null; 	
	var textChanged = null; 
	var valueChanged = null;
	this.onblur = onblur; 
	this.onfocus = onfocus;
	this.onkeypress = onkeypress; 
	this.onkeydown = onkeydown; 
	this.onkeyup = onkeyup; 
	this.SetValue = SetValue;
	this.Value = definition[18]; 
	// events	
	this.KeyDown = keyDown; 
	this.LostFocus = lostFocus; 
	this.TextChanged = textChanged; 
	this.ValueChanged = valueChanged;
	var input; 	
	function isTextValid(s, list)
	{
		if(list == null)
			list = getNumberSlots(s); 
			
		var hd = false; 
		for(var i=0;i<list.length;i++)
		{
			if(list[i].Category == 9)
				return false; 
			if(list[i].Category == 2)
				hd = true;
			if(hd && list[i].Category == 0)
			{
				if(list[i].Slot.length > definition[6] && definition[6] != -1)
					return false; 
			}
		}
		return true;		
	}
	function NumberEx(s)
	{	
		var n = 0; 	
		if(s.indexOf(definition[0]) > -1 || s.indexOf(definition[1]) > -1)
		{
			var di = -1;
			var gi = -1; 
			var post = "";
			var pre = "";
			di = s.indexOf(definition[0]); 			
			if(di > -1)
				post = s.substr(di+1); 
			gi = s.indexOf(definition[1]); 
			if(gi > -1)
			{
				var arr = s.split(definition[1]); 
				for(var i=0;i<arr.length;i++)
					pre += arr[i]; 
			}
			else if(di == -1)
				pre = s; 
			else
				pre = s.substr(0, di); 
			if(pre.length > 0)
				pre = parseInt(pre); 
			else
				pre = 0;
			if(post.length > 0)
				post = parseInt(post) * (1 / Math.pow(10, post.length)); 
			else
				post = 0; 
				
			n = pre + post; 
		}
		else
		{
			try
			{
				n = Number(s); 
			}
			catch(err)
			{ n = 0; } 
		}
		if(isNaN(n))
			n = 0;		
		return n; 
	}
	function getConvertText(s)
	{
		if(s.length == 0)
		{
			if(definition[17] == 0 || definition[17] == 1)
				return [true, null];  
		}		
		var list = getNumberSlots(s); 	
		if(!isTextValid(s, list))
			return [false, 0]; 			
		
		s = "";
		var im = -1; 
		for(var i=0;i<list.length;i++)
		{
			var ns = list[i];
			if(ns.Category == 5)
				im = 0; 
			if(ns.Category == 0 || ns.Category == 2 || ns.Category == 3)
				s += ns.Slot;
			if(ns.Category == 6)
			{
				if(im == 0)
					im = 1; 
			}
		}
		if(im == 0)
		{
			if(definition[17] == 0 || definition[17] == 1)
				return [false, null]; 
			else
				return [false, 0]; 			
		}		
		var value = 0; 
		if(s.length == 0)
			s = "0"; 
		else if(s == definition[8])
			s = "0";
		value = NumberEx(s); 
		return [true, value]; 
	}	
	function formatValue(value)
	{
		if(value == null)
			return ""; 	
		if(definition[5] == 0)
			return value;				
		var isnegative = (value < 0); 		
		if(definition[5] != 2)
			value = Math.abs(value);
		value = "" + value; 
		if(value.indexOf(".") > -1 && definition[0] != ".")
			value = value.substr(0,value.indexOf(".")) + definition[0] + value.substr(value.indexOf(".")+1); 
		var text = ""; 
		var ds = definition[0]; 			
		var gs = definition[1]; 
		var dc = definition[6];
		var gsz = definition[15]; 
		var tmp = ""; 
		var si = value.indexOf(ds);
		if(si == -1)
			tmp = value;
		else
			tmp = value.substr(0,si);
		var l = 0; 
		var i = 0; 
		var s = "";
		for(var j=tmp.length-1;j>=0;j--)
		{						
			if(gsz[l] != 0 && i == gsz[l])
			{
				s = gs + s; 
				i = 0;
				if(l + 1 < gsz.length)
					l++; 
			}
			s = tmp.substr(j,1) + s;
			i++; 
		}
		text += s;
		s = ""; 
		if(si == -1 && dc == 0)
		{ } 
		else
		{
			if(si != -1)
				s = value.substr(si+1,value.length); 
			
			while(s.length < dc)
				s += "0"; 
			text += ds;
			text += s;
		}
		if(definition[5] == 1)
		{
			var re, pattern;
			if(isnegative)
				pattern =  definition[13]; 
			else
				pattern = definition[14]; 
				
			text = pattern.replace("n", text); 
			text = text.replace("-", definition[8]); 
			text = text.replace("$", definition[2]); 			
		}
		else if(definition[5] == 3)
		{
			var re, pattern; 
			if(isnegative)
				pattern = definition[11]; 
			else
				pattern = definition[12]; 
				
			text = pattern.replace("n", text); 
			text = text.replace("-", definition[8]); 
			text = text.replace("%", definition[3]); 
		}	
		return text; 				
	}
	function SetValue(newValue)
	{
		try
		{
			var ok = false; 
			if(typeof(newValue) == "number")
			{
				this.Value = newValue;
				ok = true; 
			}
			else if(typeof(newValue) == "string")
			{
				var result = getConvertText(newValue);
				this.Value = result[1]; 
				ok = true; 				
			}
			if(ok)
			{
				input.value = formatValue(this.Value); 
				fireEvent("ValueChanged", [numericEditBox]); 
			}
		}
		catch(err)
		{ } 
	}
	function onLostFocus()
	{
		if(numericEditBox.LostFocus != null)
			numericEditBox.LostFocus();
	}
	function onblur()
	{		
		try
		{			
			var result = getConvertText(input.value); 			
			if(result[1] != this.Value)
			{
				this.Value = result[1]; 
				onValueChanged(); 
			}					
			input.value = formatValue(this.Value);	
			onLostFocus();
		}
		catch(err)
		{ }
		fireEvent("Blur", [numericEditBox]); 
	}	
	function onfocus()
	{	
		if(input == null)
			input = document.getElementById(id);
		if(definition[16] == 1)
		{
			var value = getConvertText(input.value); 
			if(value[1] == null)
				input.value = "";
			else
				input.value = value[1]; 
		}
		input.select(); 		
		fireEvent("Focus", [numericEditBox]); 
	}
	function getInputText()
	{
		return input.value; 
	}
	function getBeforeText()
	{
		if(s0 == 0)
			return "";
		else
			return getInputText().substr(0,s0);
	}
	function getAfterText()
	{
		var i = s0 + (s1-s0); 
		if(i < getInputText().length)
			return getInputText().substr(s0); 
		else
			return ""; 
	}
	function isDigit(code)
	{
		return (code >= 48 && code <= 57); 
	}
	function NumberSlot(c,s)
	{
		this.Slot = s;
		this.Category = c;
		this.Incomplete = false;		
	}
	function getNumberSlot(s, i, c)
	{
		if(s.length == 0)
			return null; 
			
		var str = s.substr(i); 
		switch(c)
		{	
			case 1:
			{
				var strGroup = definition[1]; 
				if(str.indexOf(strGroup) == 0)
					return new NumberSlot(c, strGroup); 
				else
				{
					var l = strGroup.length - 1;
					while(l > 0)
					{
						if(str.indexOf(strGroup.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c,strGroup.substr(0,l));
							ns.Incomplete = true; 
							return ns; 
						}
						else
							l--; 
					}
					return null; 
				}				
			}
			case 2: 			
			{
				var strDecimal = definition[0]; 
				if(str.indexOf(strDecimal) == 0)
					return new NumberSlot(c, strDecimal);
				else
				{
					var l = strDecimal.length - 1;
					while(l > 0)
					{
						if(str.indexOf(strDecimal.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c, strDecimal.substr(0,l));
							ns.Incomplete = true;
							return ns;
						}
						else
							l--;
					}
				}
				return null; 
			}
			case 3:
			{
				var sign = definition[7]; 
				if(str.indexOf(sign) == 0)
					return new NumberSlot(c, sign);
				else
				{
					var l = sign.length - 1;
					while(l > 0)
					{
						if(str.indexOf(sign.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c, sign.substr(0, l));
							ns.Incomplete = true;
							return ns;
						}
						else
							l--; 
					}
				}
				sign = definition[8];
				if(str.indexOf(sign) == 0)
					return new NumberSlot(c, sign);
				else
				{
					var l = sign.length - 1; 
					while(l > 0)
					{
						if(str.indexOf(sign.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c,sign.substr(0,l)); 
							ns.Incomplete = true;
							return ns;
						}
						else
							l--; 
					}
					return null; 
				}
			}
			case 4:
			{
				var strSymbol = definition[2]; 
				if(str.indexOf(strSymbol) == 0)
					return new NumberSlot(c, strSymbol); 
				else
				{
					var l = strSymbol.length - 1; 
					while(l > 0)
					{
						if(str.indexOf(strSymbol.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c, strSymbol.substr(0, l)); 
							ns.Incomplete = true;
							return ns; 
						}
						else
							l--;
					}
					return null;
				} 
			}
			case 5:
			{
				if(str.indexOf("(") == 0)
					return new NumberSlot(c, "("); 
				else
					return null; 
			}
			case 6:
			{
				if(str.indexOf(")") == 0)
					return new NumberSlot(c, ")"); 
				else
					return null; 
			}
			case 7:
			{
				var percent = definition[3]; 
				if(str.indexOf(percent) == 0)
					return new NumberSlot(c, percent); 
				else
				{
					var l = percent.length - 1; 
					while(l > 0)
					{
						if(str.indexOf(percent.substr(0,l)) == 0)
						{
							var ns = new NumberSlot(c, percent.substr(0,l)); 
							ns.Incomplete = true;
							return ns;
						}
						else
							l--;
					}
					return null; 
				}
			}
			case 8:
			{
				if(s.charCodeAt(i) == 32)
					return new NumberSlot(c, ""+s.charCodeAt(i)); 
				else
					return null; 
			}
			default:
			{
				var index = 0;
				var strDigits = "";
				while(isDigit(str.charCodeAt(index)))
				{
					strDigits += str.substr(index,1); 
					index++;
					if(!(index<str.length))
						break; 
				}
				if(strDigits.length > 0)
					return new NumberSlot(0, strDigits);
				else
					return null; 
			}
		}		
	}
	function getNumberSlots(s)
	{
		var csop = (definition[4] == 1); 
		var cscp = false;
		var css = true; 
		var cscs = (definition[5] == 1); 
		var csd = true;
		var csgs = false;
		var csds = (definition[6] != 0); 
		var csp = (definition[5] == 3);
		var hdi = false; 
		var hde = false; 
		var list = new Array(); 
		var ns = null; 
		var i = 0; 
		while(i < s.length)
		{
			if(csop)
			{
				ns = getNumberSlot(s, i, 5); 
				if(ns != null)
				{
					i += ns.Slot.length;
					csop = false;
					cscp = true;
					css = false;
					list[list.length] = ns;
					continue;
				}
			}
			if(css)
			{
				ns = getNumberSlot(s, i, 3);
				if(ns != null)
				{
					i += ns.Slot.length; 
					css = false;
					list[list.length] = ns;
					csd = !hdi;
					csop = false; 
					csp = csp & csd;
					continue;
				}
			}
			if(cscs)
			{
				ns = getNumberSlot(s, i, 4);
				if(ns != null)
				{
					i += ns.Slot.length;
					cscs = false;
					list[list.length] = ns;
					csd = !hdi;
					continue; 
				}				
			}
			if(csd)
			{
				ns = getNumberSlot(s,i,0);
				if(ns != null)
				{
					i += ns.Slot.length;
					hdi = true;
					list[list.length] = ns;
					csgs = !hde && (definition[5] != 0); 
					continue;
				}
			}
			if(csds)
			{
				ns = getNumberSlot(s, i, 2); 
				if(ns != null)
				{
					i += ns.Slot.length;
					csgs = false; 
					csds = false; 
					hde = true;
					list[list.length] = ns;
					continue; 
				}
			}
			if(csgs)
			{
				ns = getNumberSlot(s,i,1);
				if(ns != null)
				{
					i += ns.Slot.length;
					list[list.length] = ns; 
					continue;
				}
			}
			if(cscp)
			{
				ns = getNumberSlot(s,i,6); 
				if(ns != null)
				{
					i += ns.Slot.length; 
					csop = false;
					cscp = false; 
					csds = false;
					csd = false;
					csp = false;
					list[list.length] = ns;
					continue;
				}
			}
			if(csp)
			{
				var search = false; 
				if(!hdi)
					search = (definition[9] == 2 || definition[10] == 2); 
				else
					search = (definition[9] == 2 || definition[10] == 2); 
					
				ns = getNumberSlot(s,i,7); 
				if(ns != null)
				{
					i += ns.Slot.length; 
					csd = !hdi;
					csp = false; 
					csdc = !hdi; 
					csgs = !hdi && (definition[4] != 0); 
					list[list.length] = ns;
					continue; 
				}
			}
			ns = getNumberSlot(s,i,8);
			if(ns != null)
			{
				i += ns.Slot.length; 
				list[list.length] = ns; 
				csd = !hdi; 
				continue;
			}
			list[list.length] = new NumberSlot(9, s.substr(i));
			break;
		}
		return list; 
	}
	function onValueChanged()
	{
		if(numericEditBox.ValueChanged != null)
			numericEditBox.ValueChanged(); 
	}
	function onTextChanged(s)
	{
		var result = getConvertText(s); 
		if(numericEditBox.Value != result[1])
		{
			numericEditBox.Value = result[1];
			onValueChanged(); 
		}
		if(numericEditBox.TextChanged != null)
			numericEditBox.TextChanged();
	}
	var isDecimalPressed = false; 
	var keyCanceled = false;
	function onkeypress()
	{
		if(browser.isNetscape)
		{
			if(window.event.keyCode != 0)
				return; 			
		}	
		var c = fireEvent("KeyPress", [numericEditBox]); 
		if(c == null || !c)
		{			
			var schar = String.fromCharCode(getKeyCode(window.event, input)); 			
			if(isDecimalPressed && schar == ".")
				schar = definition[0]; 
			var s = getBeforeText() + schar + getAfterText(); 
			var list  =getNumberSlots(s); 
			var ci = s0; 
			var si = -1;
			var csf = 0;
			var i = 0;
			var hd = false;
			for(var j=0;j<list.length;j++)
			{
				var ns = list[j]; 
				if(ns.Category == 9)
				{					
					window.event.cancelBubble = true;
					window.event.returnValue = false; 
					keyCanceled = true;
					return;					
				}
				if(ns.Category == 2)
					hd = true;
				if(hd && ns.Category == 0)
				{
					if(ns.Slot.length > definition[6] && definition[6] != -1)
					{
						window.event.cancelBubble = true;
						window.event.returnValue = false;
						keyCanceled = true; 
						return;
					}
				}
				if(ci == csf)
					si = i;
				csf += ns.Slot.length;
				i++;				
			}
			if(si > 0 && list[si-1].Incomplete)
			{
				window.event.cancelBubble = true;
				window.event.returnValue = false; 
				keyCanceled = true; 
			}
			keyCanceled = false;						
		}
	}
	var s0, s1; 
	function onkeydown()
	{
		var c = fireEvent("KeyDown", [numericEditBox]); 
		if(c == null || !c)
		{
			var il = retrieveInputLimits(input); 
			s0 = il[0]; 
			s1 = il[1];
			if(window.event.keyCode == definition[0].charCodeAt(0))
				isDecimalPressed = true; 
		}
		if(this.KeyDown != null)
			this.KeyDown(); 
	}
	function onkeyup()
	{
		isDecimalPressed = false; 
		if(!keyCanceled)
			onTextChanged(input.value); 
		keyCanceled = false; 
	}
	function fireEvent(name, params)
	{
		return fireEventEx(events, name, params); 
	}	
	input = document.getElementById(id); 
	browser.handleEvent(input, "blur", function() { return numericeditbox_onblur(id); } );  
	browser.handleEvent(input, "focus", function() { return numericeditbox_onfocus(id); } );		
	browser.handleEvent(input, "keypress", function() { return numericeditbox_onkeypress(id); } );
	browser.handleEvent(input, "keydown", function() { return numericeditbox_onkeydown(id); } );
	browser.handleEvent(input, "keyup", function() { return numericeditbox_onkeyup(id); } );	
	fireEvent("Load", [this]); 
	var numericEditBox = this;
	return this; 	
}
function IntegerUpDown(id, definition, events)
{
	var id = id;
	var events = events;
	var numericEditBox = null; 
	var valueChanged = null;
	var readonly = (definition[5] == 1); 
	var hotcss = definition[4]; 
	this.Maximum = definition[0]; 
	this.Minimum = definition[1]; 
	this.Increment = definition[2];
	this.Value = definition[3]; 
	this.ValueChanged = valueChanged; 
	this.SetValue = SetValue; 
	this.onbuttonup = onbuttonup;
	this.onbuttondown = onbuttondown;
	this.onbuttonmouseover = onbuttonmouseover; 
	this.onlateinitialize = onlateinitialize; 
	function onbuttonup()
	{
		if(readonly)
			return; 
		var value = integerUpDown.Value;
		if(value + this.Increment > this.Maximum)
			return;
		this.SetValue(value + this.Increment, true); 
	}
	function onbuttondown()
	{		
		if(readonly)
			return; 
		var value = integerUpDown.Value;
		if(value - this.Increment < this.Minimum)
			return;		
		this.SetValue(integerUpDown.Value - this.Increment, true); 		
	}
	function getButtonElement()
	{
		var element = window.event.srcElement;
		while(element != null && element.tagName != "TD")
			element = element.parentElement;					
		return element; 
	}
	function onbuttonmouseover(type)
	{		
		if(hotcss.length > 0)
		{		
			var element =getButtonElement();
			if(type == 0)
				element.className = element.className + " " + hotcss; 				
			else
			{				
				var i = element.className.indexOf(hotcss); 
				if(i > 0)
					element.className = element.className.substr(0,i-1); 
			}
		}
	}
	function syncValue(bounds, value)
	{
		if(value == null)
			value = numericEditBox.Value;			
		if(bounds)
		{
			if(value < integerUpDown.Minimum)
				value = integerUpDown.Minimum;
			else if(value > integerUpDown.Maximum)
				value = integerUpDown.Maximum; 
		}
		if(value != integerUpDown.Value)
		{
			integerUpDown.SetValue(value, false); 
			if(value != numericEditBox.Value)
				numericEditBox.SetValue(value); 
			onvaluechanged(); 
		}
	}
	function SetValue(value, sync)
	{
		this.Value = value; 
		if(sync != null && sync)
			numericEditBox.SetValue(value);
		onvaluechanged(); 
	}
	function onvaluechanged()
	{
		if(integerUpDown.ValueChanged != null)
			integerUpDown.ValueChanged(); 
		fireEvent("ValueChanged", [integerUpDown]); 
	}
	var processTextChanged = false; 
	var arrowsHandled = false; 
	function onnumerickeydown()
	{
		if(window.event.type == "keydown")
		{
			if(window.event.keyCode == 38 || window.event.keyCode == 40)
			{
				if(readonly)
					return; 
				arrowsHandled = true; 				
				var value = numericEditBox.Value; 
				if(value == null)
				{
					syncValue(true, integerUpDown.Minimum);
					return;
				}										
				if(window.event.keyCode == 38)
					value += 1;
				else
					value -= 1;
					
				syncValue(true, value); 				
				return; 
			}
		}
	}
	function onnumerictextchanged()
	{		
		if(arrowsHandled)
		{
			arrowsHandled = false;
			return; 
		}
		processTextChanged = true; 
		syncValue(false);
	}
	function onnumericvaluechanged()
	{
		if(!processTextChanged)
			syncValue(true); 
		else
			syncValue(false);
		processTextChanged = false; 
	}
	function onnumericlostfocus()
	{
		syncValue(true); 
	}
	function onlateinitialize()
	{
		numericEditBox = getEditBox(id+"_numeric"); 
		numericEditBox.KeyDown = onnumerickeydown; 
		numericEditBox.LostFocus = onnumericlostfocus; 
		numericEditBox.TextChanged = onnumerictextchanged; 
		numericEditBox.ValueChanged = onnumericvaluechanged; 
	}
	function fireEvent(name, params)
	{
		return fireEventEx(events, name, params); 
	}
	var button = document.getElementById(id + "_bu"); 
	browser.handleEvent(button, "click", function() { return buttonup_onmousedown(id); } ); 
	if(hotcss.length > 0)
	{
		browser.handleEvent(button, "mouseover", function() { return integerupdown_onmouseover(id,0); } ); 
		browser.handleEvent(button, "mouseout", function() { return integerupdown_onmouseover(id,1); } ); 
	}
	button = document.getElementById(id + "_bd"); 
	browser.handleEvent(button, "click", function() { return buttondown_onmousedown(id); } );
	if(hotcss.length > 0)
	{
		browser.handleEvent(button, "mouseover", function() { return integerupdown_onmouseover(id,0); } ); 
		browser.handleEvent(button, "mouseout", function() { return integerupdown_onmouseover(id,1); } ); 
	}
	browser.handleEvent(window, "load", function() { return integerupdown_onlateinitialize(id); } ); 	
	fireEvent("Load", [this]); 
	var integerUpDown = this;
	return this; 
}