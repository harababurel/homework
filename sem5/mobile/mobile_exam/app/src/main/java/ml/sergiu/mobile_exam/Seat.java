package ml.sergiu.mobile_exam;

import android.arch.persistence.room.Entity;
import android.arch.persistence.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;

@Entity
public class Seat implements Serializable {
    @PrimaryKey
    public int id;
    public String name;
    public String type;
    public String status;

    public Seat() {
    }

    public Seat(JSONObject json) throws JSONException {
        id = json.getInt("id");
        name = json.getString("name");
        status = json.getString("status");
        type = json.getString("type");
    }

    @Override
    public String toString() {
        return name + " (" + type + ") - " + status;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}
