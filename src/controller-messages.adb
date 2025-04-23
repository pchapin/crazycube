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
   How_Far : String(1..Max_Length);     -- how far the drone is being asked to go
   Command : String(1..Max_Length);     -- the command being asked
   How_Far_Last : Natural := 0;
   Command_Last : Natural := 0;
   Max_Height : constant Natural := 100;
   In_Air : Boolean := False;
   Min_Height : constant Natural := 3;

   procedure Get_How_Far(Is_Number : out Boolean) is
      Current_Char : Character;
   begin
      Put_Line("");
      Put_Line("Please enter how many inches you want to go " & To_Lower(Command(1 .. Command_Last)) & ". Enter '*' to submit");
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

      if To_Lower(Command(1 .. Command_Last)) = "launch" and not In_Air then
         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "land" and In_Air then
         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "up" and In_Air then
         Get_How_Far;

            if Natural'Value(How_Far(1..How_Far_Last)) > Max_Height then
               Put_Line("");
               Put_Line("Distance is Too Far.");
               Ask_For_Command;
            end if;
            Put_Line("");
            Put_Line("Checking sensors...");
            Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Sensors,
               Request_ID => 1);
            Route_Message(Sensor_Message);
      elsif To_Lower(Command(1 .. Command_Last)) = "down" and In_Air then
         Get_How_Far;

         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "left" and In_Air then
         Get_How_Far;
         Put_Line("Going left...");

         Distance := Time_Type'Value(How_Far(1..How_Far_last));

         -- if distance is > 500 then too far!!

         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 0,
            VoltageTwo => 5,
            VoltageThree => 5,
            VoltageFour => 0,
            Time => Distance);
         Route_Message(Command_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "right" and In_Air then
         Get_How_Far;
         Put_Line("Going right...");

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

      elsif To_Lower(Command(1 .. Command_Last)) = "forward" and In_Air then
         Get_How_Far;
         Put_Line("Going forward...");

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

      elsif To_Lower(Command(1 .. Command_Last)) = "backward" and In_Air then
         Get_How_Far;
         Put_Line("Going backward...");

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

      elsif To_Lower(Command(1 .. Command_Last)) = "help" then
         Put_Line("");
         Put_Line("Available Commands Are:");
         Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Altitude, Help");
         Ask_For_Command;
      elsif To_Lower(Command(1 .. Command_Last)) = "altitude" then
         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1);
         Route_Message(Sensor_Message);

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
    with Pre => Motors.API.Is_Move_Reply(Message)
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
         Put_Line("Motors could not safely carry out command, please try again.");
         Ask_For_Command;
      end if;

   end Handle_Move_Reply;

   procedure Handle_Dumy_Altitude_Reply(Message : in Message_Record)
     with Pre => Sensors.API.Is_Get_Dumy_Altitude_Reply(Message)
   is
      Height : State_Type;
      Status : Message_Status_Type;
   begin
      Sensors.API.Get_Dumy_Altitude_Reply_Decode
        (Message => Message,
         Inches => Height,
         Decode_Status => Status);
      if To_Lower(Command(1 .. Command_Last)) = "launch" and not In_Air and Height = 0 then
         --  Put_Line("alt = " & State_Type'Image(Height));
         Put_Line("Launching motors...");
         Route_Message(Motors.API.Launch_Request_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID     => 1));
      elsif To_Lower(Command(1 .. Command_Last)) = "land" and In_Air and Height > 0 then
         Put_Line("Landing Motors");
         Route_Message(Motors.API.Land_Request_Encode
                         (Sender_Address => Name_Resolver.Motors,
                          Request_ID     => 1,
                          Time           => Time_Type'Value(State_Type'Image(Height))));
      elsif To_Lower(Command(1 .. Command_Last)) = "up" and In_Air and
        Height + State_Type'Value(How_Far(1 .. How_Far_Last)) < State_Type'Value(Natural'Image(Max_Height)) then
         Put_Line("Going up...");
         Route_Message(Motors.API.Increase_Voltage_Encode
                       (Sender_Address => Name_Resolver.Motors,
                        Request_ID => 1,
                        VoltageOne => 5,
                        VoltageTwo => 5,
                        VoltageThree => 5,
                        VoltageFour => 5,
                        Time => Time_Type'Value(How_Far(1 .. How_Far_Last))));
      elsif To_Lower(Command(1 .. Command_Last)) = "down" and In_Air and
        Height - State_Type'Value(How_Far(1 .. How_Far_Last)) > State_Type'Value(Natural'Image(Min_Height)) then
         Put_Line("Going down...");
         Route_Message(Motors.API.Decrease_Voltage_Encode
                       (Sender_Address => Name_Resolver.Motors,
                        Request_ID => 1,
                        VoltageOne => 5,
                        VoltageTwo => 5,
                        VoltageThree => 5,
                        VoltageFour => 5,
                        Time => Time_Type'Value(How_Far(1 .. How_Far_Last))));
      elsif To_Lower(Command(1 .. Command_Last)) = "altitude" then
         Put_Line("alt = " & State_Type'Image(Height));
         Ask_For_Command;
      else
         Put_Line("Command cannot be carried out. Please try again.");
         Ask_For_Command;
      end if;
   end Handle_Dumy_Altitude_Reply;

   procedure Handle_Launch_Reply(Message : in Message_Record)
     with Pre => Motors.API.Is_Launch_Reply(Message)
   is
      Status : Message_Status_Type;
      Successful : Motors.API.Status_Type;
   begin
      Motors.API.Launch_Reply_Decode
        (Message       => Message,
         Successful    => Successful,
         Decode_Status => Status);
      if Successful = Success and Status = Success then
         Put_Line("Launch was successful!");
         In_Air := True;
         Ask_For_Command;
      else
         Put_Line("Launch was not successsful, please try again!");
         Ask_For_Command;
      end if;

   end Handle_Launch_Reply;

   procedure Handle_Land_Reply(Message : in Message_Record)
     with Pre => Motors.API.Is_Land_Reply(Message)
   is
      Status : Message_Status_Type;
      Successful : Motors.API.Status_Type;
   begin
      Motors.API.Land_Reply_Decode
        (Message => Message,
         Successful => Successful,
         Decode_Status => Status);

      if Successful = Success and Status = Success then
         Put_Line("Landing was successful!");
         In_Air := False;
         Ask_For_Command;
      else
         Put_Line("Landing was not successful, please try again!");
         Ask_For_Command;
      end if;

   end Handle_Land_Reply;
   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Motors.API.Is_Move_Reply(Message) then
         Handle_Move_Reply(Message);
      elsif Sensors.API.Is_Get_Dumy_Altitude_Reply(Message) then
         Handle_Dumy_Altitude_Reply(Message);
      elsif Motors.API.Is_Launch_Reply(Message) then
         Handle_Launch_Reply(Message);
      elsif Motors.API.Is_Land_Reply(Message) then
         Handle_Land_Reply(Message);
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
      Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Altitude, Help");
      Ask_For_Command;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Controller.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Controller.Messages;
