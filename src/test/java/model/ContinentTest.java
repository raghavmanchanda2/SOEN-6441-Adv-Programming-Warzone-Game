package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.Continent;
import model.Country;

class ContinentTest {
	
	private int uniqueContinentId = 1;
	private String continentID = "Asia";
	private String contientValue;
	private List<Country> continentCountriesT3, continentCountriesT4;
	
	Continent CTest, CTest3, CTest4;

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
		System.out.println("Inside setup before class");
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
		System.out.println("Inside tear down after class");
	}

	@BeforeEach
	void setUp() throws Exception {
		System.out.println("Inside set up");
		CTest = new Continent(1, "Asia");
		
		
		//Test 3
		
		continentCountriesT3 = new ArrayList<Country>();
		continentCountriesT3.add(new Country("Canada"));
		continentCountriesT3.add(new Country("USA"));
		continentCountriesT3.add(new Country("Mexico"));
		
		
		List<Country> CountryListT3 = new ArrayList<Country>();
		CountryListT3.add(new Country("Canada"));
		CountryListT3.add(new Country("USA"));
		CountryListT3.add(new Country("Mexico"));
		
		CTest3 = new Continent("North America", CountryListT3);
		
		//Test 4
		
		
		continentCountriesT4 = new ArrayList<Country>();
		
		continentCountriesT4.add(new Country("China"));
		continentCountriesT4.add(new Country("Japan"));
		continentCountriesT4.add(new Country("South Korea"));
		
		CTest4 = new Continent("Asia", continentCountriesT3);
		
		
		//
	}

	@AfterEach
	void tearDown() throws Exception {
		System.out.println("tear down");
	}

	@Test
	void Test1() {
		assertEquals(uniqueContinentId, CTest.getUniqueContinetId());
	}
	
	@Test
	void Test2() {
		uniqueContinentId = 2;
		CTest.setUniqueContinetId(2);
		assertEquals(uniqueContinentId, CTest.getUniqueContinetId());
	}
	
	@Test
	void Test3() {
		
		for(int i = 0; i < continentCountriesT3.size() && i < CTest3.getContinentCountries().size(); ++i) {
			assertEquals(continentCountriesT3.get(i).getCountryId(), CTest3.getContinentCountries().get(i).getCountryId());
		}
	}
	
	@Test
	void Test4() {
		
		List<Country> CountryListT4 = new ArrayList<Country>();
		CountryListT4.add(new Country("China"));
		CountryListT4.add(new Country("Japan"));
		CountryListT4.add(new Country("South Korea"));
		
		CTest4.setContinentCountries(CountryListT4);
		
		for(int i = 0; i < continentCountriesT4.size() && i < CTest4.getContinentCountries().size(); ++i) {
			assertEquals(CTest4.getContinentCountries().get(i).getCountryId(), continentCountriesT4.get(i).getCountryId());
		}
		
	}
	
	@Test
	void Test5() {
		assertEquals(continentID, CTest.getContinentId());
		//fail("Not yet implemented");
	}
	
	
	

}
























