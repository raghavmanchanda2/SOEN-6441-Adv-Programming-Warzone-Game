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

	private static final int MAX_CARD_LIMIT = 3;
	
	private String playerName;
	//private int armiesHold = 5; // base armies
	private int baseArmies = 3;
	private int reinforcementArmies = 0; // holding countries / 3 down to
	private int bonusArmies = 0; // continent 
	private int currentArmies = baseArmies + reinforcementArmies + bonusArmies;
	private boolean d_can_get_card_this_turn;

	private List<Country> countriesHold;
	private Map<Country,Integer> currentArmyInCountry;
	private List<Card> d_cards = new ArrayList<>();
	
	private List<Order> orders_list;
	
	private Player peaceWith;
	
	public Player(String playerName) {
		this.playerName = playerName;
		d_can_get_card_this_turn = true;
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
	
	
	public void removeCountryHold(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		if(this.currentArmyInCountry == null) {
			this.currentArmyInCountry = new HashMap<>();
		}
		this.countriesHold.remove(country);
		this.currentArmyInCountry.remove(country);
	}
	
	
	
	//-------------------------------------------
	public void addCountry(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		
		this.countriesHold.add(country);
		country.setCountryOwner(this);
	}
	
	public void removeCountry(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		
		this.countriesHold.remove(country);
	}
	
	public Country getCountry(Country p_country) {
		for(Country country : countriesHold) {
			if(country == p_country) {
				return country;
			}
		}
		return null;
	}
	
	public Player getPeaceWith() {
		return peaceWith;
	}
	
	public void setPeaceWith(Player player) {
		peaceWith = player;
	}
	
	public void resetPeaceWith() {
		peaceWith = null;
	}
	
	//-------------------------------------------
	
	
	
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
