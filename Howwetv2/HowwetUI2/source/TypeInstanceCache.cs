using System;
using System.Collections;


namespace APSRU.Howwet
    {
    internal class TypeInstanceCache
        {
        private Hashtable cache = new Hashtable();

        /// <summary>
        /// Return an instance of the given type.
        /// </summary>
        /// <remarks>
        /// If this call has never been made for this type before, a new instance of that type is created,
        /// added to the cache, and returned. Otherwise, if a call has been made for this type before, then the
        /// previously created instance is returned.
        /// </remarks>
        /// <param name="type">
        /// The type of instance to obtain from the cache. The type must support a no-arg constructor or
        /// a MissingMethodException will be thrown.
        /// </param>
        /// <returns>An instance of the desired type</returns>
        public object GetInstanceOfType(Type type)
            {
            // Attempt to retrieve an instance of the type from the cache, using the type as a key.
            object o = cache[type];

            if (o == null)
                {
                // No instance of the type has been created yet. Create one now.
                o = Activator.CreateInstance(type);

                // Add the instance to the cache so it can be returned next time.
                cache[type] = o;
                }

            return o;
            }
        }
    }
