package com.evotek.storage.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Objects;

@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "storage")
public class FileEntity extends AuditableEntity {

    @Id
    @Column(name = "id", nullable = false, length = 36)
    private String id;

    /**
     * The file path.
     */
    @Column(name = "path", length = 250)
    private String path;

    /**
     * The file size.
     */
    @Column(name = "file_size")
    private Long size;

    /**
     * The file type.
     */
    @Column(name = "type", length = 100)
    private String type;

    /**
     * The original name.
     */
    @Column(name = "original_name", length = 200)
    private String originalName;

    /**
     * The owner id.
     */
    @Column(name = "owner_id", length = 36)
    private String ownerId;

    /**
     * The owner type.
     */
    @Column(name = "owner_type", length = 50)
    private String ownerType;

    /**
     * The hashed by md5
     */
    @Column(name = "hashed", length = 32)
    private String hashed;

    /**
     * The status.
     */
    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    /**
     * The version file
     */
    @Column(name = "version")
    private Long version;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        FileEntity that = (FileEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
