package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.player.PlayerStrategy;
import persistence.MapFileAlteration;

/**
 * Test case to ensure the amount of reinforcement armies are properly deployed
 *  
 * @author spankeydamankey
 * @version build 1
 */
class PlayerTest {
	
	Player P;
	MapFileAlteration d_MFA;
	
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
	
	/**
	 * Test to verify that if all continents are being captured then the bonus armies should be given to player
	 * The test is performed with the following steps described below
	 * 1. Create a single continent and multiple countries
	 * 2. Add those elements to the map
	 * 3. Call the calculateReinforcementArmies() method
	 * 4. Compare the the expected reinforcement armies with the value that is defined in the continent value
	 * 5. Currently only comparing with reinforcement army equals to 3
	 */
	@Test
	void calculateReinforcement() {
		d_MFA=new MapFileAlteration();
		Continent l_Asia;
		
		Country l_China, l_India, l_Japan, l_Korea;
		
		l_Asia = new Continent(2,"Asia");
		
		l_China = new Country("China", l_Asia);
		l_India = new Country("India", l_Asia);
		l_Japan = new Country("Japan", l_Asia);
		l_Korea = new Country("Korea", l_Asia);
		
		P = new Player(PlayerStrategy.getStrategy("human"));
		P.setName("Ishan");
		P.setCapturedCountries(Arrays.asList(l_China,l_India,l_Japan,l_Korea));
	
		d_MFA.getMapModel().setD_CurrentPlayer(P);
		int armyValue;
		P.calculateReinforcementArmies(d_MFA.getMapModel());
		armyValue=P.getReinforcementArmies();
		assertEquals(armyValue,3);
	}

}
