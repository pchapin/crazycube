--------------------------------------------------------------------------------
-- FILE   : Controller-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with CubedOS.Log_Server.API;
with Ada.Text_IO; use Ada.Text_IO;
with Motors.API; use Motors.API;
with Sensors.API; use Sensors.API;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Controller.Messages is
   use Message_Manager;
   Max_Length : constant Natural := 10; -- for command length
   How_Far : String(1..Max_Length);
   Command : String(1..Max_Length);
   How_Far_Last : Natural := 0;
   Command_Last : Natural := 0;
   Max_Height : constant Natural := 100;
   -- Min_Height : constant Natural := 3;

   procedure Get_How_Far is
      Current_Char : Character;
   begin
      How_Far_Last := 0;

         loop
            Get(Current_Char);
            if Current_Char = '*' then
               exit;
            end if;

         if How_Far_Last < Max_Length then
            How_Far_Last := How_Far_Last + 1;
            if Current_Char in '0' .. '9' then
               How_Far(How_Far_Last) := Current_Char;
            else
               Put_Line("");
               Put_Line("You must enter a number!");
               Put_Line("Aborting Command!");
               loop
                  Get(Current_Char);
                  if Current_Char = '*' then
                     exit;
                  end if;
               end loop;
               Ask_For_Command;
            end if;
         else
            Put_Line("");
            Put_Line("Buffer overflow. Maximum length exceeded.");
            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;
            end loop;
            Ask_For_Command;
         end if;
      end loop;
   end Get_How_Far;

   procedure Ask_For_Command is
      Distance : Motors.API.Time_Type; -- Distance in inches
      Command_Message : Message_Record;
      Sensor_Message : Message_Record;
      Current_Char : Character;
   begin
      Command := [others => ' '];
      Command_Last := 0;
      Put_Line("");
      Put_Line("Please enter the command that you want the drone to follow. Enter '*' to submit");
      loop
         Get(Current_Char);
         if Current_Char = '*' then
            exit;
         end if;

         if Command_Last < Max_Length then
            Command_Last := Command_Last + 1;
            Command(Command_Last) := Current_Char;
         else
            Ada.Text_IO.Put_Line("Buffer overflow. Maximum length exceeded.");
            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;
            end loop;

            Command := [others => ' '];
            Command_Last := 0;
            exit;
         end if;
      end loop;

      if To_Lower(Command(1 .. Command_Last)) = "launch" then
            Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);
            --  Command := [others => ' '];
            --  Last := 0;
            --  Command_Message := Motors.API.Increase_Voltage_Encode
            --    (Sender_Address => Name_Resolver.Motors,
            --     Request_ID => 1,
            --     VoltageOne => 5,
            --     VoltageTwo => 5,
            --     VoltageThree => 5,
            --     VoltageFour => 5,
            --     Time => 3);
            --  Route_Message(Command_Message);
            --  Put_Line("Message Has Been Sent");

      elsif To_Lower(Command(1 .. Command_Last)) = "land" then
         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "up" then
            Put_Line("");
            Put_Line("Please enter how far you want to go up. Enter '*' to submit");
            Get_How_Far;
            if Natural'Value(How_Far(1..How_Far_Last)) > Max_Height then
               Put_Line("");
               Put_Line("Distance is Too Far.");
               Ask_For_Command;
            end if;

            --  Distance := Time_Type'Value(How_Far(1..last));
            --  Command := [others => ' '];
            --  Last := 0;
            --  Command_Message := Motors.API.Increase_Voltage_Encode
            --    (Sender_Address => Name_Resolver.Motors,
            --     Request_ID => 1,
            --     VoltageOne => 5,
            --     VoltageTwo => 5,
            --     VoltageThree => 5,
            --     VoltageFour => 5,
            --     Time => Distance);
            --  Route_Message(Command_Message);
            Put_Line("");
            Put_Line("Checking sensors...");
            Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Sensors,
               Request_ID => 1);
            Route_Message(Sensor_Message);
      elsif To_Lower(Command(1 .. Command_Last)) = "down" then
         Put_Line("");
         Put_Line("Please enter how far you want to go down. Enter '*' to submit");
         Get_How_Far;

         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);
            --  Distance := Time_Type'Value(How_Far(1..last));
            --  Command := [others => ' '];
            --  Last := 0;
            --  Command_Message := Motors.API.Decrease_Voltage_Encode
            --    (Sender_Address => Name_Resolver.Motors,
            --     Request_ID => 1,
            --     VoltageOne => 5,
            --     VoltageTwo => 5,
            --     VoltageThree => 5,
            --     VoltageFour => 5,
            --     Time => Distance);
            --  Route_Message(Command_Message);
      elsif To_Lower(Command(1 .. Command_Last)) = "left" then
         Put_Line("");
         Put_Line("Please enter how far you want to go left. Enter '*' to submit");
         Get_How_Far;

         Distance := Time_Type'Value(How_Far(1..How_Far_last));

         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 0,
            VoltageTwo => 5,
            VoltageThree => 5,
            VoltageFour => 0,
            Time => Distance);
         Route_Message(Command_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "right" then
         Put_Line("");
         Put_Line("Please enter how far you want to go right. Enter '*' to submit");
         Get_How_Far;

         Distance := Time_Type'Value(How_Far(1..How_Far_last));

         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 5,
            VoltageTwo => 0,
            VoltageThree => 0,
            VoltageFour => 5,
            Time => Distance);
         Route_Message(Command_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "forward" then
         Put_Line("");
         Put_Line("Please enter how far you want to go forward. Enter '*' to submit");
         Get_How_Far;

         Distance := Time_Type'Value(How_Far(1..How_Far_last));

         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 0,
            VoltageTwo => 0,
            VoltageThree => 5,
            VoltageFour => 5,
            Time => Distance);
         Route_Message(Command_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "backward" then
         Put_Line("");
         Put_Line("Please enter how far you want to go backward. Enter '*' to submit");
         Get_How_Far;

         Distance := Time_Type'Value(How_Far(1..How_Far_last));

         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 5,
            VoltageTwo => 5,
            VoltageThree => 0,
            VoltageFour => 0,
            Time => Distance);
         Route_Message(Command_Message);

      --  elsif To_Lower(Command(1 .. Command_Last)) = "exit" then
      --     if not In_Air then
      --        Put_Line("");
      --        Put_Line("Thank you for flying the CrazyFlie2.1!!! :)");
      --     else
      --        Put_Line("");
      --        Put_Line("Drone has to be landed in order to exit.");
      --        Ask_For_Command;
      --     end if;
      elsif To_Lower(Command(1 .. Command_Last)) = "help" then
         Put_Line("");
         Put_Line("Available Commands Are:");
         Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Exit, Help");
         Ask_For_Command;
      else
         Put_Line("");
         Put_Line("Error, can not carry out command. Please Try again");
         Ask_For_Command;
      end if;
   end Ask_For_Command;


   -------------------
   -- Message Handling
   -------------------

   -- Here is where the details of handing the messages is done. Probably there will be a
   -- separate subprogram for each message, but the exact organization is, obviously, dependent
   -- on the needs of the module. It might be useful to put these message handling subprograms
   -- into a private sibling package. That would move the complex details of message handling to
   -- a different file and reduce clutter in this file. This file is really just about decoding
   -- and dispatching the messages. We recommend that if a single internal package is used that
   -- it should be called Sample_Module.Core (for example).

   procedure Handle_Move_Reply(Message : in Message_Record)
    -- with Pre => Motors.API.Is_Move_Reply(Message)
   is
      Status : Message_Status_Type;
      Successful : Motors.API.Status_Type;
   begin
      Motors.API.Move_Reply_Decode
        (Message,
         Successful,
         Status);
      if Successful = Success then
         Put_Line("Success!");
         Ask_For_Command;
      else
         Put_Line("Motors could not carry out command, please try again.");
         Ask_For_Command;
      end if;

   end Handle_Move_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Motors.API.Is_Move_Reply(Message) then
         Handle_Move_Reply(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Controller,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;
      -- When this procedure returns the message loop will immediately try to receive the next
      -- message. Note that all CubedOS send operations are non-blocking so sending an outgoing
      -- message will not delay execution.
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      Put_Line("Welcome to the CrazyFlie2.1 drone. This system uses CubedOS. The available commands are:");
      Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Exit, Help");
      Ask_For_Command;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Controller.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Controller.Messages;
