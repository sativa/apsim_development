using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Error
    {
    public class CustomError
        {
        private String errorNumber="";
        private String publicMessage="";
        private String techMessage="";
        private String functionName="";
        private String className="";
        private bool isCritical=false;

        public CustomError(String errorNumber,String publicMessage, String techMessage,String functionName,String className,bool isCritical)
            {
            this.errorNumber = errorNumber;
            this.publicMessage = publicMessage;
            this.techMessage = techMessage;
            this.functionName = functionName;
            this.className = className;
            this.isCritical = isCritical;
            }

        public String ErrorNumber
            {
            get { return errorNumber; }
            }
        public String PublicMessage
            {
            get { return publicMessage; }
            }
        public String TechMessage
            {
            get { return techMessage; }
            }
        public String FunctionName
            {
            get { return functionName; }
            }
        public String ClassName
            {
            get { return className; }
            }
        public bool IsCritical
            {
            get { return isCritical; }
            }
        }
    }
