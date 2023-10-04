package model;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.player.PlayerStrategy;

/**
 * Test case to ensure the amount of reinforcement armies are properly deployed
 *  
 * @author spankeydamankey
 * @version build 1
 */
class PlayerTest {
	
	Player P;

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
		
		P = new Player(PlayerStrategy.getStrategy("human"));
		P.setReinforcementArmies(3);
	}

	@AfterEach
	void tearDown() throws Exception {
		P = null;
	}
	
	/**
	 * In this example, player P has 3 armies in the reinforcement pool.
	 * We call the method deployReinforcementArmiesFromPlayer and pass the value 4 to it.
	 * This boolean method should return false if the deployed armies is larger than
	 * what the player possesses in its reinforcement pool.
	 */
	@Test
	void maxReinforcement() {
		int army = 4;
		
		assertFalse(P.deployReinforcementArmiesFromPlayer(army));
	}

}
