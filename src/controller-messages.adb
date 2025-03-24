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
with Motors.API;

package body Controller.Messages is
   use Message_Manager;

   procedure Ask_For_Command is
      Max_Command_Length : constant Natural := 50;
      Command : String(1..Max_Command_Length);
      Last : Natural := 0;
      Current_Char : Character;
      Distance : constant Float := 5.0; -- Distance in inches
      Command_Message : Message_Record;
   begin
      Put_Line("Please enter the command that you want the drone to follow. Enter '*' to submit");
      loop
      Ada.Text_IO.Get(Current_Char);
      if Current_Char = '*' then
         exit;
      end if;

      if Last < Max_Command_Length then
         Last := Last + 1;
         Command(Last) := Current_Char;
      else
         Ada.Text_IO.Put_Line("Buffer overflow. Maximum length exceeded.");
         exit;
      end if;
   end loop;

      if Command(1 .. Last) = "launch" then
         Put_Line("Launching");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "land" then
         Put_Line("Landing");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Decrease_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "up" then
         Put_Line("Going up");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "down" then
         Put_Line("Going down");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Decrease_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "left" then
         Put_Line("Going left");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
           VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "right" then
         Put_Line("Going right");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "forward" then
         Put_Line("Going forward");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      elsif Command(1 .. Last) = "backward" then
         Put_Line("Going backward");
         Command := [others => ' '];
         Last := 0;
         Command_Message := Motors.API.Increase_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => Distance,
            VoltageTwo => Distance,
            VoltageThree => Distance,
            VoltageFour => Distance);
         Route_Message(Command_Message);
      else
         Put_Line("Error, command is not valid");
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
      Success : Boolean;
   begin
      Motors.API.Move_Reply_Decode
        (Message,
         Success,
         Status);
      if Success then
         Put_Line("Success!");
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
      Ask_For_Command;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Controller.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Controller.Messages;
