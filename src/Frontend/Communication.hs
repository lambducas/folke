module Frontend.Communication (
    startCommunication,
    evaluateProofString,
    evaluateProofSegment,
    evaluateProofStep
) where

import Control.Concurrent (Chan, forkIO, writeChan, readChan)
import Shared.Messages
import Logic.Abs (Sequent, Step)
import Backend.TypeChecker (handleFrontendMessage)  -- Import the function

-- Function to start the communication between frontend and backend
startCommunication :: Chan FrontendMessage -> Chan BackendMessage -> IO BackendMessage
startCommunication frontendChan backendChan = do
    _ <- forkIO $ communicationLoop frontendChan backendChan
    return (OtherBackendMessage "Initialized")

-- Communication loop to handle messages from the frontend and backend
communicationLoop :: Chan FrontendMessage -> Chan BackendMessage -> IO ()
communicationLoop frontendChan backendChan = do
    msg <- readChan frontendChan
    let response = handleFrontendMessage msg
    writeChan backendChan response
    communicationLoop frontendChan backendChan

-- Function to evaluate a proof in text-format by communicating with the backend
evaluateProofString :: Chan FrontendMessage -> Chan BackendMessage -> String -> IO BackendMessage
evaluateProofString frontendChan backendChan text = do
    -- Send the sequent to the backend for evaluation
    writeChan frontendChan (CheckStringSequent text)
    -- Wait for the backend to respond with the result
    readChan backendChan

-- Function to evaluate a segment of the proof by communicating with the backend
evaluateProofSegment :: Chan FrontendMessage -> Chan BackendMessage -> Sequent -> IO BackendMessage
evaluateProofSegment frontendChan backendChan sequent = do
    -- Send the sequent to the backend for evaluation
    writeChan frontendChan (CheckSequent sequent)
    -- Wait for the backend to respond with the result
    readChan backendChan

-- Function to evaluate a single step of the proof by communicating with the backend
evaluateProofStep :: Chan FrontendMessage -> Chan BackendMessage -> Step -> IO BackendMessage
evaluateProofStep frontendChan backendChan step = do
    -- Send the step to the backend for evaluation
    writeChan frontendChan (CheckStep step)
    -- Wait for the backend to respond with the result
    readChan backendChan
    -- response <- readChan backendChan
    -- case response of
    --     StepChecked result -> return result
    --     _ -> return (Left "Unexpected response from backend")
