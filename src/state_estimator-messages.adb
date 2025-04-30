--------------------------------------------------------------------------------
-- FILE   : state_estimator-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2025 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;    -- See the comments in state_estimater-api.ads.
with Name_Resolver;      -- See the comments in state_estimater-api.ads.
with State_Estimator.API; use State_Estimator.API; -- Needed so that the types in the API can be used here.
with CubedOS.Log_Server.API;
with Ada.Numerics.Elementary_Functions;

package body State_Estimator.Messages is
   use Message_Manager;
   -- keeps contant track of the pitch, roll, yaw, velocity, and altitude
   Pitch : State_Type := 0;
   Roll  : State_Type := 0;
   Yaw   : State_Type := 0;
   Velocity : State_Type := 0;
   Altitude: State_Type := 0;

   -------------------
   -- Message Handling
   -------------------

   -- uses the dumy altitude to get a dumy state
   -- still routes the standard state reply message to be used by the controller
   procedure Handle_Get_Dumy_State_Request(Message : in Message_Record)
     with Pre => State_Estimator.API.Is_Get_State_Request(Message)
   is
      Status : Message_Status_Type;
      Alt : State_Type;
      State_Reply : Message_Record;
   begin
      State_Estimator.API.Get_Dumy_State_Request_Decode
        (Message        => Message,
         Dumy_Altitude  => Alt,
         Decode_Status  => Status);

      State_Reply := state_estimator.API.Get_State_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID => 1,
         AttitudeRoll => 0,
         AttitudePitch => 0,
         AttitudeYaw => 0,
         Altitude => Alt);
      Route_Message (Message => State_Reply);

   end Handle_Get_Dumy_State_Request;

   procedure Handle_Get_State_Request(Message : in Message_Record)
     with Pre => State_Estimator.API.Is_Get_State_Request(Message)
   is
      Status : Message_Status_Type;
      GyroX : Sensor_Type;
      GryoY : Sensor_Type;
      GyroZ : Sensor_Type;
      AccelX : Sensor_Type;
      AccelY : Sensor_Type;
      AccelZ : Sensor_Type;
      Accel_Pitch : Float;
      Accel_Roll : Float;

      DT : constant State_Type := 1; -- Time step

      State_Reply : Message_Record;
   begin
      State_Estimator.API.Get_State_Request_Decode
        (Message        => Message,
         GyroscopeX     => GyroX,
         GyroscopeY     => GryoY,
         GyroscopeZ     => GyroZ,
         AccelerometerX => AccelX,
         AccelerometerY => AccelY,
         AccelerometerZ => AccelZ,
         Decode_Status  => Status);

      Accel_Pitch := 180.0 * Ada.Numerics.Elementary_Functions.Arctan(Float'Value(Sensor_Type'Image(-AccelX))/100.00,
                                                                      Ada.Numerics.Elementary_Functions.Sqrt((Float'Value(Sensor_Type'Image(AccelY))/100.00) *
                                                                          (Float'Value(Sensor_Type'Image(AccelY))/100.00) + (Float'Value(Sensor_Type'Image(AccelZ))/100.00) *
                                                                            (Float'Value(Sensor_Type'Image(AccelZ)))/100.00))/Ada.Numerics.Pi;
      Accel_Roll  := 180.0 * Ada.Numerics.Elementary_Functions.Arctan((Float'Value(Sensor_Type'Image(AccelY))/100.00), (Float'Value(Sensor_Type'Image(AccelZ))/100.00))/Ada.Numerics.Pi;

      Roll := Roll + (State_Type'Value(Sensor_Type'Image(GyroX))/100) * DT;
      Pitch := Pitch + (State_Type'Value(Sensor_Type'Image(GryoY))/100) * DT;

      Roll := 98 * Roll + (-98) * State_Type'Value(Float'Image((Accel_Roll*100.00)));
      Pitch := 98 * Pitch + (-98) * State_Type'Value(Float'Image((Accel_Pitch*100.00)));
      Yaw := DT * State_Type'Value(Sensor_Type'Image(GyroZ));

      Velocity := Velocity + State_Type'Value(Sensor_Type'Image(AccelZ)) * DT;
      Altitude := Altitude + Velocity * DT;


      State_Reply := state_estimator.API.Get_State_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID => 1,
         AttitudeRoll => Roll,
         AttitudePitch => Pitch,
         AttitudeYaw => Yaw,
         Altitude => Altitude);
      Route_Message (Message => State_Reply);

   end Handle_Get_State_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process(Message : in Message_Record) is
   begin
      if State_Estimator.API.Is_Get_State_Request(Message) then
         Handle_Get_State_Request(Message);
      elsif State_Estimator.API.Is_Get_Dumy_State_Request(Message) then
         Handle_Get_Dumy_State_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.State_Estimator,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;

   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(Name_Resolver.State_Estimator.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end State_Estimator.Messages;
