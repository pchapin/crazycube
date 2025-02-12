--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the CrazyCube application.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with CubedOS.Log_Server.Messages;
with CubedOS.Time_Server.Messages;
with Controller.Messages;
with Motors.Messages;
with Sensor.Messages;
with State_Estimator.Messages;

pragma Unreferenced(CubedOS.Log_Server.Messages);
pragma Unreferenced(CubedOS.Time_Server.Messages);
pragma Unreferenced(Controller.Messages);
pragma Unreferenced(Motors.Messages);
pragma Unreferenced(Sensor.Messages);
pragma Unreferenced(State_Estimator.Messages);

procedure Main is
   pragma Priority(System.Priority'First);
begin
   null;
end Main;
