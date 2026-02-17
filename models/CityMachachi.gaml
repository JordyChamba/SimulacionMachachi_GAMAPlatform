model MachachiCity

global {
    file road_shapefile <- file("shapes/Machachi_line.shp");
    file shelter_shapefile <- file("shapes/MachachiPS.shp");
    geometry shape <- envelope(road_shapefile);
    
    graph road_network;
    graph panic_network;
    map<road,float> weights_map;
    
    int nb_people <- 900;
    
    float nivel_ceniza <- 0.0;
    float velocidad_ceniza <- 0.025;
    
    float tiempo_inicio <- 0.0;
    float tiempo_total_evacuacion <- -1.0;
    point eruption_center <- {world.shape.width * 0.48, world.shape.height * 0.93};
    float ash_ring_max_radius <- world.shape.width * 2.0;
    
    int nb_evacuated <- 0;
    int nb_dead <- 0;
    int nb_en_peligro <- 0;
    float porcentaje_salvos <- 0.0;
    float porcentaje_muertos <- 0.0;
    float velocidad_promedio_poblacion <- 0.0;
    float congestion_red <- 0.0;
    
    init {
        create road from: road_shapefile;
        create shelter from: shelter_shapefile;
        
        road_network <- as_edge_graph(road, 100.0);
        panic_network <- as_edge_graph(road, 10.0);
        
        weights_map <- road as_map (each :: each.shape.perimeter);
        
        create people number: nb_people {
            location <- any_location_in(one_of(road));
        }
        
        ask people {
            shelter s <- shelter closest_to self;
            mi_refugio <- s;
            
            if (s != nil) {
                if (path_between(road_network, location, s.location) = nil) {
                    loop times: 10 {
                        location <- any_location_in(one_of(road));
                        s <- shelter closest_to self;
                        mi_refugio <- s;
                        if (path_between(road_network, location, s.location) != nil) { break; }
                    }
                }
            }
            
            if (mi_refugio != nil and path_between(road_network, location, mi_refugio.location) = nil) {
                location <- any_location_in(one_of(road at_distance 100));
                if (path_between(road_network, location, mi_refugio.location) = nil) {
                    location <- any_location_in(one_of(road closest_to mi_refugio));
                }
            }
            
            if (mi_refugio != nil) {
                point centro <- mi_refugio.location;
                point direccion <- location - centro;
                float distancia_actual <- location distance_to centro;
                
                if (distancia_actual > 0) {
                    // Normalización manual (sin operador normalized_to)
                    float longitud <- sqrt(direccion.x * direccion.x + direccion.y * direccion.y);
                    point vector_normalizado <- direccion / longitud;
                    target <- centro + vector_normalizado * 130.0;
                } else {
                    target <- centro;
                }
            }
            
            do add_desire(in_shelter);
        }
        
        create ceniza_display;
        tiempo_inicio <- time;
    }
    
    reflex subir_ceniza {
        nivel_ceniza <- min(100.0, nivel_ceniza + velocidad_ceniza);
        ask people { my_panic <- nivel_ceniza / 100.0; }
    }
    
    reflex update_weights when: every(10#cycle) {
        weights_map <- road as_map (each :: each.shape.perimeter / max(0.1, each.speed_coeff));
        road_network <- road_network with_weights weights_map;
        panic_network <- panic_network with_weights weights_map;
        congestion_red <- 1.0 - (road mean_of (each.speed_coeff));
    }
    
    reflex actualizar_metricas when: every(1#cycle) {
        nb_evacuated <- people count (each.color = #lime);
        nb_dead <- people count (each.color = #magenta);
        nb_en_peligro <- nb_people - nb_evacuated - nb_dead;
        
        if (nb_people > 0) {
            porcentaje_salvos <- (nb_evacuated * 100.0) / nb_people;
            porcentaje_muertos <- (nb_dead * 100.0) / nb_people;
            velocidad_promedio_poblacion <- people mean_of (each.speed);
        }
        
        if (nb_evacuated + nb_dead = nb_people and tiempo_total_evacuacion < 0) {
            tiempo_total_evacuacion <- time - tiempo_inicio;
            do pause;
        }
    }
    
    reflex mostrar_resultados when: every(500#cycle) or (nb_evacuated + nb_dead = nb_people) {
        write "════════ METRICAS DE EVACUACIÓN ════════";
        write " SALVADOS: " + nb_evacuated + " (" + string(int(porcentaje_salvos)) + "%)";
        write " MUERTOS: " + nb_dead + " (" + string(int(porcentaje_muertos)) + "%)";
        write " VELOCIDAD MEDIA: " + string(round(velocidad_promedio_poblacion * 100)/100.0);
        write " CONGESTIÓN RED: " + string(round(congestion_red * 100 * 10)/10.0) + "%";
        if (tiempo_total_evacuacion >= 0) {
            write " TIEMPO FINAL: " + string(int(tiempo_total_evacuacion / 1000)) + " seg";
        }
        write "════════════════════════════════════════";
    }
}

species ash_ring {
    float radius <- 70 + rnd(100);
    float speed <- 1.2 + rnd(1.0);
    float rx_factor <- 1.2 + rnd(1.6);
    float ry_factor <- 0.6 + rnd(1.2);
    float ang <- rnd(360.0);
    float fade_start <- ash_ring_max_radius * 0.6;
    float current_alpha <- 40.0;
    
    init { radius <- radius + rnd(80); }
    
    reflex expand {
        radius <- radius + speed;
        if (radius > fade_start) {
            float fade_progress <- (radius - fade_start) / (ash_ring_max_radius - fade_start);
            current_alpha <- 40.0 * (1.0 - fade_progress);
        }
        if (radius > ash_ring_max_radius + rnd(300) - 150) { do die; }
    }
    
    aspect default {
        int alpha <- int(max(3, current_alpha));
        rgb ash_color <- rgb(110, 110, 140, alpha);
        draw ellipse(radius * rx_factor * 2, radius * ry_factor * 2) 
            at: eruption_center rotate: ang color: ash_color border: ash_color.darker;
    }
}

species ceniza_display {
    reflex emitir_anillo when: every(100#cycle) { 
        if (length(ash_ring) < 10) { create ash_ring number: 1; } 
    }
    
    reflex limpiar_exceso when: every(30#cycle) {
        if (length(ash_ring) > 25) {
            // Versión segura y compatible
            list<ash_ring> a_eliminar <- first(8, (ash_ring sort_by (each.current_alpha)));
            ask a_eliminar { do die; }
        }
    }
}

species people skills: [moving] control: simple_bdi {
    point target <- nil;
    shelter mi_refugio <- nil;
    predicate in_shelter <- new_predicate("in_shelter");
    float my_panic <- 0.0;
    rgb color <- #yellow;
    float tiempo_expuesto <- 0.0;
    float tiempo_maximo_en_rojo <- 500.0;
    bool muerto <- false;
    
    plan evacuar intention: in_shelter {
        if (muerto) { return; }
        
        if (color = #lime) {
            speed <- 0.0;
            return;
        }
        
        color <- my_panic < 0.3 ? #yellow : (my_panic < 0.7 ? #orange : #red);
        
        // Velocidad progresiva → reduce saltos
        float base <- 0.45;
        speed <- base + (0.75 * my_panic) - (0.4 * max(0.0, my_panic - 0.65));
        // Ej: 0.0 → ~0.45   0.5 → ~0.825   0.7 → ~0.90   1.0 → ~0.675
        
        if (flip(0.1) or every(20#cycle)) {  // chequea ocasionalmente (10% de probabilidad o cada 20 ciclos)
        shelter nuevo_cercano <- shelter closest_to self;
        if (nuevo_cercano != nil and nuevo_cercano != mi_refugio) {
            float dist_nueva <- self distance_to nuevo_cercano.location;
            float dist_actual <- target != nil ? self distance_to target : 99999.0;
            
            if (dist_nueva < dist_actual * 0.7) {  // si el nuevo es al menos 30% más cerca
                mi_refugio <- nuevo_cercano;
                point centro <- nuevo_cercano.location;
                point dir_vec <- location - centro;
                float dist <- location distance_to centro;
                
                if (dist > 0) {
                    point unit <- dir_vec / dist;
                    target <- centro + unit * 130.0;
                } else {
                    target <- centro;
                }
            }
        }
    }
    
        do goto target: target
            on: (my_panic > 0.7 ? panic_network : road_network)
            move_weights: weights_map;
        
        if (mi_refugio != nil) {
            if (self distance_to mi_refugio.location <= 135.0) {
                color <- #lime;
                speed <- 0.0;
                do remove_intention(in_shelter, true);
            }
        }
    }
    
    reflex contar_tiempo_en_peligro when: !muerto and color = #red {
        tiempo_expuesto <- tiempo_expuesto + 1;
        if (tiempo_expuesto > tiempo_maximo_en_rojo) {
            color <- #magenta;
            speed <- 0.0;
            muerto <- true;
            do remove_intention(in_shelter, false);
        }
    }
    
    reflex salvarse when: color = #lime {
        tiempo_expuesto <- 0.0;
        muerto <- false;
    }
    
    // Impulso suave si se queda muy lento cerca del target
    reflex anti_atrapamiento when: every(25#cycle) {
        if (target != nil and location distance_to target < 20 and speed < 0.15 and !muerto) {
            speed <- speed * 1.4;
        }
    }
    
    aspect default { 
        draw triangle(50) rotate: heading + 90 color: color border: #black; 
    }
}

species road {
    // speed_coeff más suave para evitar paradas totales repentinas
    float speed_coeff <- 1.0 update: max(0.35, 
        1.0 / (1.0 + exp(5.0 * (length(people at_distance 4.0) / (shape.perimeter + 0.1) - 0.65)))
    );
    
    aspect default { draw shape color: #black width: 2; }
}

species shelter {
    aspect default { draw circle(130) color: #gamablue border: #white width: 8; }
}

experiment MachachiCity type: gui {
    parameter "Población Inicial" var: nb_people min: 50 max: 24000;
    parameter "Crecimiento Ceniza" var: velocidad_ceniza min: 0.01 max: 0.2;
    
    output {
        display Mapa type: 2d background: #white {
            species ash_ring;
            species ceniza_display;
            species road;
            species shelter;
            species people;
        }
        
        display "Proporción de Supervivencia" type: 2d {
            chart "Composición de la Población" type: pie background: #white {
                data "Salvados" value: nb_evacuated color: #green;
                data "Fallecidos" value: nb_dead color: #red;
                data "En Tránsito" value: nb_en_peligro color: #yellow;
            }
        }
        
        display "Dinámica de Movimiento" type: 2d {
            chart "Rendimiento de la Red" type: series background: #white {
                data "Nivel de Congestión (%)" value: congestion_red * 100 color: #orange;
                data "Velocidad Promedio" value: velocidad_promedio_poblacion color: #blue;
            }
        }
        
        display "Progreso de Evacuación" type: 2d {
            chart "Curva Acumulativa" type: series background: #white {
                data "Total Evacuados" value: nb_evacuated color: #green thickness: 3;
                data "Total Fallecidos" value: nb_dead color: #magenta thickness: 3;
            }
        }
        
        display "Indicadores Clave" type: 2d {
            chart "Métricas Numéricas" type: series {
                data "Tasa de Supervivencia %" value: porcentaje_salvos color: #green;
            }
            graphics "Info" {
                draw "Evacuados: " + nb_evacuated at: {10, 20} color: #black font: font("Arial", 18, #bold);
                draw "Fallecidos: " + nb_dead at: {10, 40} color: #red font: font("Arial", 18, #bold);
                draw "Tiempo: " + (tiempo_total_evacuacion > 0 ? string(int(tiempo_total_evacuacion/1000)) : "En proceso") 
                    at: {10, 60} color: #blue font: font("Arial", 18, #bold);
            }
        }
    }
}