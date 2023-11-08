package controller;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import logger.GeneralException;
import model.ResponseWrapper;

class SingleGameModePlayEngineControllerTest {
	
	private SingleGameModePlayEngineController singleGameModePlayEngineController;
	
	@BeforeEach
	void setUp() {
		singleGameModePlayEngineController = new SingleGameModePlayEngineController();
	}
	
	@Test
	void getPlaySetupInvalidCommandsTest() throws GeneralException {
		
		String passBombCommand = "bomb Pakistan";
		ResponseWrapper responseOfBombCommandPlaySetUpPhase = singleGameModePlayEngineController.getPlaySetupCommands(passBombCommand);
		singleGameModePlayEngineController.getPlaySetupCommands("bomb").getDescription();
		ResponseWrapper expectedResponse = new ResponseWrapper(404, "Incorrect Command in Current State");
		assertEquals(responseOfBombCommandPlaySetUpPhase.getDescription(),expectedResponse.getDescription());
		assertEquals(responseOfBombCommandPlaySetUpPhase.getStatusValue(),expectedResponse.getStatusValue());
	}

}
