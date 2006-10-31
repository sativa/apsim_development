using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using APSRU.Error;


namespace APSRU.Error
    {
    public class CustomException : Exception
        {
        private ArrayList errors = new ArrayList();
        public CustomException(CustomError error)
            {
            errors.Add(error);
            }
        public ArrayList getErrors()
            {
            return errors;
            }
        public void addError(CustomError error)
            {
            errors.Add(error);
           
            }
        }
    }
