package model;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class GameModelTest {

	GameModel GM = GameModel.getInstance(); 
	
	private Player P1, P2;
	
	@BeforeEach
	void setUp() throws Exception {
		P1 = new Player("Kevin");
		P2 = new Player("Rohit");
		
		GM.addPlayerInPlayersList(P1);
		GM.addPlayerInPlayersList(P2);
		
		GM.addPlayerQueue(P1);
		GM.addPlayerQueue(P2);
	}

	@Test
	void getNextPlayer() {
		
		Player P = GM.getNextPlayer();
		P = GM.getNextPlayer();
		
		assertEquals(P2, P);
		
	}

}
