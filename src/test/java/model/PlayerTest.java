package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.player.PlayerStrategy;
import persistence.MapFileAlteration;

/**
 * Test case to ensure the amount of reinforcement armies are properly calculated and deployed
 *  
 * @author Kevin
 * @version build 1
 */
class PlayerTest {
	Player P;
	MapFileAlteration d_MFA;
	
	MapModel MM;
	
	Continent d_Asia;
	Country d_China, d_India, d_Japan, d_Korea;
	
	Continent d_America;
	Country d_Canada, d_USA, d_Mexico, d_Guatemala, d_Nicaragua, d_Colombia, d_Venezuela, d_Ecuador, d_Peru, d_Brazil;
	
	List<Country> d_SmallCapturedCountries = new ArrayList<>();
	List<Country> d_LargeCapturedCountries = new ArrayList<>();
	
	Player P1, P2, P3;

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
		
		P1 = new Player(PlayerStrategy.getStrategy("human"));
		P1.setReinforcementArmies(3);
		
		P2 = new Player(PlayerStrategy.getStrategy("human"));
		P3 = new Player(PlayerStrategy.getStrategy("human"));
		
		P1.setName("Ishaan");
		P2.setName("Rohit");
		P3.setName("Raghav");
		
		MM = new MapModel();
		
		//Asia
		d_Asia = new Continent(2,"Asia","30");
		d_Asia.setD_AwardArmies(100);
		
		d_China = new Country("China", d_Asia);
		d_India = new Country("India", d_Asia);
		d_Japan = new Country("Japan", d_Asia);
		d_Korea = new Country("Korea", d_Asia);
		
		MM.addContinent(d_Asia);
		
		MM.addContinentCountries(d_Asia, d_China);
		MM.addContinentCountries(d_Asia, d_India);
		MM.addContinentCountries(d_Asia, d_Japan);
		MM.addContinentCountries(d_Asia, d_Korea);
		
		MM.addBorders(d_India, d_China);
		
		MM.addBorders(d_China, d_India);
		MM.addBorders(d_China, d_Japan);
		MM.addBorders(d_China, d_Korea);
		
		MM.addBorders(d_Japan, d_China);
		
		MM.addBorders(d_Korea, d_China);
		
		//America
		d_America = new Continent(1,"America","10");
		d_America.setD_AwardArmies(200);
		
		d_Canada = new Country("Canada", d_America);
		d_USA = new Country("USA", d_America);
		d_Mexico = new Country("Mexico", d_America);
		d_Guatemala = new Country("Guatemala", d_America);
		d_Nicaragua = new Country("Nicaragua", d_America);
		d_Colombia = new Country("Colombia", d_America);
		d_Venezuela = new Country("Venezuela", d_America);
		d_Ecuador = new Country("Ecuador", d_America);
		d_Peru = new Country("Peru", d_America);
		d_Brazil = new Country("Brazil", d_America);
		
		
		MM.addContinent(d_America);
		
		MM.addContinentCountries(d_America, d_Canada);
		MM.addContinentCountries(d_America, d_USA);
		MM.addContinentCountries(d_America, d_Mexico);
		MM.addContinentCountries(d_America, d_Guatemala);
		MM.addContinentCountries(d_America, d_Nicaragua);
		MM.addContinentCountries(d_America, d_Colombia);
		MM.addContinentCountries(d_America, d_Venezuela);
		MM.addContinentCountries(d_America, d_Ecuador);
		MM.addContinentCountries(d_America, d_Peru);
		MM.addContinentCountries(d_America, d_Brazil);
		
		MM.addBorders(d_Canada, d_USA);
		
		MM.addBorders(d_USA, d_Canada);
		MM.addBorders(d_USA, d_Mexico);
		
		MM.addBorders(d_Mexico, d_USA);
		MM.addBorders(d_Mexico, d_Guatemala);
		
		MM.addBorders(d_Guatemala, d_Mexico);
		MM.addBorders(d_Guatemala, d_Nicaragua);
		
		MM.addBorders(d_Nicaragua, d_Guatemala);
		MM.addBorders(d_Nicaragua, d_Colombia);
		
		MM.addBorders(d_Colombia, d_Nicaragua);
		MM.addBorders(d_Colombia, d_Venezuela);
		MM.addBorders(d_Colombia, d_Ecuador);
		MM.addBorders(d_Colombia, d_Brazil);
		MM.addBorders(d_Colombia, d_Peru);
		
		MM.addBorders(d_Venezuela, d_Colombia);
		MM.addBorders(d_Venezuela, d_Brazil);
		
		MM.addBorders(d_Ecuador, d_Colombia);
		MM.addBorders(d_Ecuador, d_Peru);
		
		MM.addBorders(d_Peru, d_Ecuador);
		MM.addBorders(d_Peru, d_Colombia);
		MM.addBorders(d_Peru, d_Brazil);
		
		MM.addBorders(d_Brazil, d_Peru);
		MM.addBorders(d_Brazil, d_Colombia);
		MM.addBorders(d_Brazil, d_Venezuela);
		
		
		
		d_SmallCapturedCountries.add(d_China);
		d_SmallCapturedCountries.add(d_India);
		d_SmallCapturedCountries.add(d_Japan);
		d_SmallCapturedCountries.add(d_Korea);
		
		P2.setCapturedCountries(d_SmallCapturedCountries);
		
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		d_LargeCapturedCountries.add(d_Ecuador);
		
		P3.setCapturedCountries(d_LargeCapturedCountries);
		
	}

	@AfterEach
	void tearDown() throws Exception {
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
		
		assertFalse(P1.deployReinforcementArmiesFromPlayer(army));
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
