package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import business.Order.Order;

/**
 * Player class which holds the issue order and next order in the list of orders
 * @author Rohit
 * @version build 2
 */
public class Player {

	private String playerName;
	//private int armiesHold = 5; // base armies
	private int baseArmies = 3;
	private int reinforcementArmies = 0; // holding countries / 3 down to
	private int bonusArmies = 0; // continent 
	private int currentArmies = baseArmies + reinforcementArmies + bonusArmies;
	

	private List<Country> countriesHold;
	private Map<Country,Integer> currentArmyInCountry;
	
	private List<Order> orders_list;
	
	public Player(String playerName) {
		this.playerName = playerName;
	}
	
	
	public List<Country> getCountriesHold() {
		return countriesHold;
	}


	public Map<Country, Integer> getCurrentArmyInCountry() {
		return currentArmyInCountry;
	}


	public void setCurrentArmyInCountry(Map<Country, Integer> currentArmyInCountry) {
		this.currentArmyInCountry = currentArmyInCountry;
	}


	public void setCountriesHold(List<Country> countriesHold) {
		this.countriesHold = countriesHold;
	}

	public void addCountryHold(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		if(this.currentArmyInCountry == null) {
			this.currentArmyInCountry = new HashMap<>();
		}
		this.countriesHold.add(country);
		this.currentArmyInCountry.put(country, 0);
	}

	
	public String getPlayerName() {
		return playerName;
	}


	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	public int getCurrentArmies() {
		return currentArmies;
	}


	public void setCurrentArmies(int currentArmies) {
		this.currentArmies = currentArmies;
	}
	
	
	
	public void addOrder(Order order) {
		if(orders_list == null) {
			this.orders_list = new ArrayList();
		}
		this.orders_list.add(order);
	}
	
	public Order getNextOrder() {
		
		if (!this.orders_list.isEmpty()) {
			
			Order to_return = this.orders_list.get(0); 
			this.orders_list.remove(0);
			
			return to_return;
		} else
			return null;
	}
	
}
