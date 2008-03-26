using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Howwet
    {
    public interface IEventListener
        {
        void OnNotification(IHowwetModel publisher);
        }
    }
