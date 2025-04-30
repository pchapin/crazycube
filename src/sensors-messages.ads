--------------------------------------------------------------------------------
-- FILE   : sensors-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2025 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;

package Sensors.Messages is

   task Message_Loop is
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end Sensors.Messages;
