module Frontend.Communication (
    startCommunication,
    evaluateProofString,
    evaluateProofFE
) where

import Control.Concurrent (Chan, forkIO, writeChan, readChan)
import Shared.Messages
import Backend.TypeChecker (handleFrontendMessage)  -- Import the function
import Shared.FESequent (FESequent)

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

evaluateProofFE :: Chan FrontendMessage -> Chan BackendMessage -> FESequent -> IO BackendMessage
evaluateProofFE frontendChan backendChan tree = do
    -- Send the sequent to the backend for evaluation
    writeChan frontendChan (CheckFESequent tree)
    -- Wait for the backend to respond with the result
    readChan backendChan