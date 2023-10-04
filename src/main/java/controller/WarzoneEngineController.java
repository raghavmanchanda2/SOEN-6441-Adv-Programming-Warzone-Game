package controller;

import java.util.InputMismatchException;
import java.util.Scanner;

public class WarzoneEngineController {
	
	private Scanner d_inputForFeatureSelection;
	
	
	public String getGameFeatureInputs() {
		String l_selectedFeature = "";
		try {
			
			d_inputForFeatureSelection = new Scanner(System.in);
			l_selectedFeature = d_inputForFeatureSelection.nextLine();
			
		
		}catch(InputMismatchException p_inputMismatchException){
			// throws global exception
			System.out.println("Please provide proper input");
			return "404";
		}

		return l_selectedFeature;

	}
	
	
}	
