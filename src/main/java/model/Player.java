package model;

import java.util.*;

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
	private int baseArmies = 5;
	private int armiesToIssue = 5;
	private int reinforcementArmies = 0; // holding countries / 3 down to
	private int bonusArmies = 0; // continent 
	private int currentArmies = baseArmies + reinforcementArmies + bonusArmies;
	private boolean d_can_get_card_this_turn;

	private List<Country> countriesHold;
	private Map<Country,Integer> currentArmyInCountry;
	private List<Card> d_cards = new ArrayList<>();

	private Deque<Order> orders_list = new ArrayDeque<>();

	private List<Order> cardOrders_list;
	
	private Player peaceWith;
	
	public Player(String playerName) {
		this.playerName = playerName;
		d_can_get_card_this_turn = true;
	}

	public int getArmiesToIssue() {
		return armiesToIssue;
	}

	public void setArmiesToIssue(int armiesToIssue) {
		this.armiesToIssue = armiesToIssue;
	}

	public Order nextOrder() {
		return orders_list.poll();
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
	
	public List<Card> getCardList() {
		return d_cards;
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

	public Deque<Order> getOrders() {
		return orders_list;
	}

	public void setOrders(Deque<Order> d_Orders) {
		this.orders_list = d_Orders;
	}

	public void addOrder(Order order) {
		if(orders_list == null) {
			this.orders_list = new ArrayDeque<>();
		}
		this.orders_list.add(order);
	}
	

	
}
