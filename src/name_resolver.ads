--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2025 by Vermont State University
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

   -- Core Modules
   Log_Server  : constant Message_Address := (0, 1);
   Time_Server : constant Message_Address := (0, 2);

   -- Application-Specific Modules
   Sensors         : constant Message_Address := (0, 3);
   State_Estimator : constant Message_Address := (0, 4);
   Controller      : constant Message_Address := (0, 5);
   Motors          : constant Message_Address := (0, 6);
end Name_Resolver;
