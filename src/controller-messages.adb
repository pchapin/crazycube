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
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Controller.Messages is
   use Message_Manager;
   In_Air : Boolean := false;

   procedure Land is
      Command_Message : Message_Record;
   begin
      Put_Line("landing...");
      Command_Message := Motors.API.Decrease_Voltage_Encode
           (Sender_Address => Name_Resolver.Motors,
            Request_ID => 1,
            VoltageOne => 5,
            VoltageTwo => 5,
            VoltageThree => 5,
            VoltageFour => 5,
            Time => 5);
      In_Air := false;
      Route_Message(Command_Message);
   end Land;

   procedure Ask_For_Command is
      Max_Length : constant Natural := 10;
      Command : String(1..Max_Length);
      How_Far : String(1..Max_Length);
      Last : Natural := 0;
      Current_Char : Character;
      Distance : Motors.API.Time_Type; -- Distance in inches
      Command_Message : Message_Record;
      Max_Distance : constant Natural := 100;
   begin
      Put_Line("");
      Put_Line("Please enter the command that you want the drone to follow. Enter '*' to submit");
      loop
         Get(Current_Char);
         if Current_Char = '*' then
            exit;
         end if;

         if Last < Max_Length then
            Last := Last + 1;
            Command(Last) := Current_Char;
         else
            Ada.Text_IO.Put_Line("Buffer overflow. Maximum length exceeded.");
            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;
            end loop;

            Command := [others => ' '];
            Last := 0;
            exit;
         end if;
      end loop;

      if To_Lower(Command(1 .. Last)) = "launch" then
         if not In_Air then
            Put_Line("");
            Put_Line("Launching");
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 5,
               VoltageTwo => 5,
               VoltageThree => 5,
               VoltageFour => 5,
               Time => 5);
            In_Air := true;
            Route_Message(Command_Message);
            Put_Line("Message Has Been Sent");
         else
            Put_Line("");
            Put_Line("Drone is already in the air.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "land" then
         if In_Air then
            Put_Line("");
            Command := [others => ' '];
            Last := 0;
            Land;
         else
            Put_Line("");
            Put_Line("Drone is already landed");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "up" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go up. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Up " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Up " & Current_Char & " inches.");
                     end if;
                     How_Far(Last) := Current_Char;
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

            if Natural'Value(How_Far(1..last)) > Max_Distance then
               Put_Line("");
               Put_Line("Distance is Too Far.");
               Ask_For_Command;
            end if;

            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 5,
               VoltageTwo => 5,
               VoltageThree => 5,
               VoltageFour => 5,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "down" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go down. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Down " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Down " & Current_Char & " inches.");
                     end if;
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

            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Decrease_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 5,
               VoltageTwo => 5,
               VoltageThree => 5,
               VoltageFour => 5,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "left" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go left. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Left " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Left " & Current_Char & " inches.");
                     end if;
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



            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 0,
               VoltageTwo => 5,
               VoltageThree => 5,
               VoltageFour => 0,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "right" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go right. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Right " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Right " & Current_Char & " inches.");
                     end if;
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

            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 5,
               VoltageTwo => 0,
               VoltageThree => 0,
               VoltageFour => 5,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "forward" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go forward. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Forward " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Forward " & Current_Char & " inches.");
                     end if;
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

            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 0,
               VoltageTwo => 0,
               VoltageThree => 5,
               VoltageFour => 5,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "backward" then
         if In_Air then
            Put_Line("");
            Put_Line("Please enter how far you want to go backward. Enter '*' to submit");
            Last := 0;

            loop
               Get(Current_Char);
               if Current_Char = '*' then
                  exit;
               end if;

               if Last < Max_Length then
                  Last := Last + 1;
                  if Current_Char in '0' .. '9' then
                     if Current_Char = '1' then
                        Put_Line("");
                        Put_Line("Going Backward " & Current_Char & " inch.");
                     else
                        Put_Line("");
                        Put_Line("Going Backward " & Current_Char & " inches.");
                     end if;
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

            Distance := Time_Type'Value(How_Far(1..last));
            Command := [others => ' '];
            Last := 0;
            Command_Message := Motors.API.Increase_Voltage_Encode
              (Sender_Address => Name_Resolver.Motors,
               Request_ID => 1,
               VoltageOne => 5,
               VoltageTwo => 5,
               VoltageThree => 0,
               VoltageFour => 0,
               Time => Distance);
            Route_Message(Command_Message);
         else
            Put_Line("");
            Put_Line("Drone must be in the air. Launch the drone.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "exit" then
         if not In_Air then
            Put_Line("");
            Put_Line("Thank you for flying the CrazyFlie2.1!!! :)");
         else
            Put_Line("");
            Put_Line("Drone has to be landed in order to exit.");
            Ask_For_Command;
         end if;
      elsif To_Lower(Command(1 .. Last)) = "help" then
         Put_Line("");
         Put_Line("Available Commands Are:");
         Put_Line("Launch, Land, Up, Down, Left, Right, Forward, Backward, Exit, Help");
         Ask_For_Command;
      else
         Put_Line("");
         Put_Line("Error, can not carry out command.");
         if In_Air then
            Land;
         end if;
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
