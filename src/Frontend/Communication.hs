module Frontend.Communication (
    startCommunication,
    evaluateProofFE
) where

import Control.Concurrent (Chan, forkIO, writeChan, readChan)
import Shared.Messages
import Backend.TypeChecker (handleFrontendMessage)  -- Import the function
import Shared.FESequent (FEDocument)

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

evaluateProofFE :: Chan FrontendMessage -> Chan BackendMessage -> FEDocument -> Bool -> Int -> IO BackendMessage
evaluateProofFE frontendChan backendChan doc acpFlag wrngSensetivity= do
    -- Send the sequent to the backend for evaluation
    writeChan frontendChan (CheckFEDocument (doc, (wrngSensetivity, acpFlag)))
    -- Wait for the backend to respond with the result
    readChan backendChan