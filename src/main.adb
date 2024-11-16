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

pragma Unreferenced(CubedOS.Log_Server.Messages);
pragma Unreferenced(CubedOS.Time_Server.Messages);

procedure Main is
   pragma Priority(System.Priority'First);
begin
   null;
end Main;
