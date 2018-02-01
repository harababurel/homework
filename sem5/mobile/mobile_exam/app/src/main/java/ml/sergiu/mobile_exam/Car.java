package ml.sergiu.mobile_exam;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;

public class Car implements Serializable {
    public int id;
    public String name;
    public int quantity;
    public String type;
    public String status;

    public Car() {
    }

    public Car(JSONObject json) throws JSONException {
        id = json.getInt("id");
        name = json.getString("name");
        quantity = json.getInt("quantity");
        status = json.getString("status");
        type = json.getString("type");
    }

    @Override
    public String toString() {
        return "Car{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", quantity=" + quantity +
                ", type='" + type + '\'' +
                ", status='" + status + '\'' +
                '}';
    }
}
