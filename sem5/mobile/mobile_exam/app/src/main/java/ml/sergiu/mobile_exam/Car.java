package ml.sergiu.mobile_exam;

import org.json.JSONException;
import org.json.JSONObject;

public class Car {
    public int id;
    public String name;
    public int quantity;
    public String type;
    public String status;

    public Car(JSONObject json) throws JSONException {
        id = json.getInt("id");
        name = json.getString("name");
        quantity = json.getInt("quantity");
        status = json.getString("status");
        type = json.getString("type");
    }
}
