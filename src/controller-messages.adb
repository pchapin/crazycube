--------------------------------------------------------------------------------
-- FILE   : Controller-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2025 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with CubedOS.Log_Server.API;
with Ada.Text_IO; use Ada.Text_IO;
with Motors.API; use Motors.API;
with Sensors.API; use Sensors.API;
with State_Estimator.API; use State_Estimator.API;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Controller.Messages is
   use Message_Manager;
   Max_Length : constant Natural := 10; -- for string length
   How_Far : String(1..Max_Length);     -- how far the drone is being asked to go
   Command : String(1..Max_Length);     -- the command being asked
   How_Far_Last : Natural := 0;         -- to keep track of the last char in How_Far
   Command_Last : Natural := 0;         -- to keep track of the last char in Command
   Max_Height : constant Natural := 100;-- the max height the drone can hover
   In_Air : Boolean := False;           -- true if in air, false if not
   Min_Height : constant Natural := 3;  -- the min height that the drone can hover

   -- gets the input for how far the user wants to fly the drone
   -- Is_Number is true if the user inputs a number and false if not
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
               Is_Number := False;
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

   -- asks for the command that the user wants to carry out.
   -- handles all casses correctly
   procedure Ask_For_Command is
      Distance : Motors.API.Time_Type; -- Distance in inches
      Command_Message : Message_Record;
      Sensor_Message : Message_Record;
      Current_Char : Character;
      Is_Number : Boolean := True;
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
           (Sender_Address => Name_Resolver.Controller,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "land" and In_Air then
         Put_Line("");
         Put_Line("Checking sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Controller,
            Request_ID => 1);
         Route_Message(Sensor_Message);

      elsif To_Lower(Command(1 .. Command_Last)) = "up" and In_Air then
         Get_How_Far(Is_Number);

         if Is_Number then
            if Natural'Value(How_Far(1..How_Far_Last)) > Max_Height then
               Put_Line("");
               Put_Line("Distance is Too Far. Aborting command!");
               Ask_For_Command;
            end if;
            Put_Line("");
            Put_Line("Checking sensors...");
            Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Controller,
               Request_ID => 1);
            Route_Message(Sensor_Message);
         else
            Put_Line("You must enter a number. Aborting command!");
            Ask_For_Command;
         end if;

      elsif To_Lower(Command(1 .. Command_Last)) = "down" and In_Air then
         Get_How_Far(Is_Number);

         if Is_Number then
            Put_Line("");
            Put_Line("Checking sensors...");
            Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Controller,
               Request_ID => 1);
            Route_Message(Sensor_Message);
         else
            Put_Line("You must enter a number. Aborting command!");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Command_Last)) = "left" and In_Air then
         Get_How_Far(Is_Number);
         if Is_Number then

            if Integer'Value(How_Far(1..How_Far_last)) < 100 then
               Distance := Time_Type'Value(How_Far(1..How_Far_last));
               Put_Line("Going left...");
               Command_Message := Motors.API.Increase_Voltage_Encode
                 (Sender_Address => Name_Resolver.Controller,
                  Request_ID => 1,
                  VoltageOne => 0,
                  VoltageTwo => 5,
                  VoltageThree => 5,
                  VoltageFour => 0,
                  Time => Distance);
               Route_Message(Command_Message);
            else
               Put_Line("Distance is too far. Aborting command!");
               Ask_For_Command;
            end if;

         else
            Put_Line("You must enter a number. Aborting command!");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Command_Last)) = "right" and In_Air then
         Get_How_Far(Is_Number);
         if Is_Number then

            if Integer'Value(How_Far(1..How_Far_last)) < 100 then
               Distance := Time_Type'Value(How_Far(1..How_Far_last));
               Put_Line("Going right...");
               Command_Message := Motors.API.Increase_Voltage_Encode
                 (Sender_Address => Name_Resolver.Controller,
                  Request_ID => 1,
                  VoltageOne => 5,
                  VoltageTwo => 0,
                  VoltageThree => 0,
                  VoltageFour => 5,
                  Time => Distance);
               Route_Message(Command_Message);
            else
               Put_Line("Distance is too far. Aborting command!");
               Ask_For_Command;
            end if;

         else
            Put_Line("You must enter a number. Aborting command!");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Command_Last)) = "forward" and In_Air then
         Get_How_Far(Is_Number);
         if Is_Number then

            if Integer'Value(How_Far(1..How_Far_last)) < 100 then
               Distance := Time_Type'Value(How_Far(1..How_Far_last));
               Put_Line("Going forward...");
               Command_Message := Motors.API.Increase_Voltage_Encode
                 (Sender_Address => Name_Resolver.Controller,
                  Request_ID => 1,
                  VoltageOne => 0,
                  VoltageTwo => 0,
                  VoltageThree => 5,
                  VoltageFour => 5,
                  Time => Distance);
               Route_Message(Command_Message);
            else
               Put_Line("Distance is too far. Aborting command!");
               Ask_For_Command;
            end if;

         else
            Put_Line("You must enter a number. Aborting command!");
            Ask_For_Command;
         end if;

      elsif To_Lower(Command(1 .. Command_Last)) = "backward" and In_Air then
         Get_How_Far(Is_Number);
         if Is_Number then

            if Integer'Value(How_Far(1..How_Far_last)) < 100 then
               Distance := Time_Type'Value(How_Far(1..How_Far_last));
               Put_Line("Going backward...");
               Command_Message := Motors.API.Increase_Voltage_Encode
                 (Sender_Address => Name_Resolver.Controller,
                  Request_ID => 1,
                  VoltageOne => 5,
                  VoltageTwo => 5,
                  VoltageThree => 0,
                  VoltageFour => 0,
                  Time => Distance);
               Route_Message(Command_Message);
            else
               Put_Line("Distance is too far. Aborting command!");
               Ask_For_Command;
            end if;

         else
            Put_Line("You must enter a number. Aborting Command.");
            Ask_For_Command;
         end if;

      elsif To_Lower(Command(1 .. Command_Last)) = "help" then
         Put_Line("");
         Put_Line("Available Commands Are:");
         Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Altitude, Help");
         Ask_For_Command;
      elsif To_Lower(Command(1 .. Command_Last)) = "altitude" then
         Put_Line("");
         Put_Line("Checking alt sensors...");
         Sensor_Message := Sensors.API.Get_Dumy_Altitude_Request_Encode
           (Sender_Address => Name_Resolver.Controller,
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
      Altitude : Sensors.API.State_Type;
      Status : Message_Status_Type;
   begin
      Sensors.API.Get_Dumy_Altitude_Reply_Decode
        (Message => Message,
         Inches => Altitude,
         Decode_Status => Status);

      Put_Line("Estimating state...");
      Route_Message( state_estimator.API.Get_Dumy_State_Request_Encode
                     (Sender_Address => Name_Resolver.Controller,
                      Request_ID => 1,
                      Dumy_Altitude => state_estimator.API.State_Type'Value(sensors.API.State_Type'Image(Altitude))));
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

   -- once the state is obtained, the command is checked and handled correctly
   procedure Handle_Get_State_Reply(Message : in Message_Record)
     with Pre => state_estimator.API.Is_Get_State_Reply(Message)
   is
      Status : Message_Status_Type;
      Roll : state_estimator.API.State_Type;
      Pitch : state_estimator.API.State_Type;
      Yaw : state_estimator.API.State_Type;
      Altitude : state_estimator.API.State_Type;
   begin
      state_estimator.API.Get_State_Reply_Decode
        (Message => Message,
         AttitudeRoll => Roll,
         AttitudePitch => Pitch,
         AttitudeYaw => Yaw,
         Altitude => Altitude,
         Decode_Status => Status);
      -- Check if the drone is stable.
      -- Outside the simulation, you would likely want to be more forgiving.
      if Roll = 0 and Pitch = 0 and Yaw = 0 then

         if To_Lower(Command(1 .. Command_Last)) = "altitude" then
            Put_Line("Drone altitude = " & state_estimator.API.State_Type'Image(Altitude));
            Ask_For_Command;
         elsif To_Lower(Command(1 .. Command_Last)) = "launch" and not In_Air and Altitude = 0 then
            Put_Line("Launching motors...");
            Route_Message(Motors.API.Launch_Request_Encode
                          (Sender_Address => Name_Resolver.Controller,
                           Request_ID     => 1));
         elsif To_Lower(Command(1 .. Command_Last)) = "land" and In_Air and Altitude > 0 then
            Put_Line("Landing Motors");
            Route_Message(Motors.API.Land_Request_Encode
                          (Sender_Address => Name_Resolver.Controller,
                           Request_ID     => 1,
                           Time           => Time_Type'Value(state_estimator.API.State_Type'Image(Altitude))));
         elsif To_Lower(Command(1 .. Command_Last)) = "up" and In_Air and
           Altitude + state_estimator.API.State_Type'Value(How_Far(1 .. How_Far_Last)) < state_estimator.API.State_Type'Value(Natural'Image(Max_Height)) then
            Put_Line("Going up...");
            Route_Message(Motors.API.Increase_Voltage_Encode
                          (Sender_Address => Name_Resolver.Controller,
                           Request_ID => 1,
                           VoltageOne => 5,
                           VoltageTwo => 5,
                           VoltageThree => 5,
                           VoltageFour => 5,
                           Time => Time_Type'Value(How_Far(1 .. How_Far_Last))));
         elsif To_Lower(Command(1 .. Command_Last)) = "down" and In_Air and
           Altitude - state_estimator.API.State_Type'Value(How_Far(1 .. How_Far_Last)) > state_estimator.API.State_Type'Value(Natural'Image(Min_Height)) then
            Put_Line("Going down...");
            Route_Message(Motors.API.Decrease_Voltage_Encode
                          (Sender_Address => Name_Resolver.Controller,
                           Request_ID => 1,
                           VoltageOne => 5,
                           VoltageTwo => 5,
                           VoltageThree => 5,
                           VoltageFour => 5,
                           Time => Time_Type'Value(How_Far(1 .. How_Far_Last))));
         else
            Put_Line("Command cannot be carried out. Please enter new command!");
            Ask_For_Command;
         end if;
      else
         Put_Line("Command cannot be carried out. Please enter new command!");
         Ask_For_Command;
      end if;

   end Handle_Get_State_Reply;

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
      elsif state_estimator.API.Is_Get_State_Reply(Message) then
         Handle_Get_State_Reply(Message);
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
