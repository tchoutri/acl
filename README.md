# ACL

This package models an Access Control List (ACL) system with Google's [Zanzibar] as its main inspiration.

Examples are taken from Okta's [OpenFGA Sandbox].

[Zanzibar]: https://storage.googleapis.com/gweb-research2023-media/pubtools/5068.pdf
[OpenFGA]: https://play.fga.dev/sandbox/?store=entitlements

## Terminology

ACL entries are three-tuples of an object, a relation and a subject (called subject in the Zanzibar paper).

A subject is the entity whose access of the object is being controlled. This is very close to how the grammar of some Western European languages work: subject, verb and object

The way to represent tuples is the following:

```
⟨tuple⟩ = ⟨object⟩#⟨relation⟩@⟨subject⟩

⟨object⟩ = ⟨namespace⟩:⟨object-id⟩

⟨subject⟩ = ⟨subject-id⟩ | ⟨subject-set⟩

⟨subject-set⟩ = ⟨object⟩#⟨relation⟩
```

Subjects can be directly identified by their subject identifier (`⟨subject-id`), or by a `⟨subject-set⟩`. From the Zanzibar paper:

> [⟨subject-set⟩] allows ACLs to refer to groups and thus supports representing nested group membership

### Examples

<dl>
    <dt> 1. doc:readme#owner@user:10
    <dd> User 10 has the <b>owner</b> relationship to the <b>readme</b> object in the <b>doc</b> namespace
</dl>

<dl>
    <dt> 2. group:eng#member@user:11
    <dd> User 11 has the <b>member</b> relationship to the <b>eng</b> object in the <b>group</b> namespace
</dl>

<dl>
    <dt> 3. doc:readme#viewer@group:eng#member
    <dd> Members of group <b>eng</b> are <b>viewers</b> of <b>doc:readme</b>
</dl>

## Relationships

### Direct relationships

The building block of an ACL is the declaration of a relationship between a subject and an object.

See Example 1.

### Computed relationships

In order to avoid storage waste, it is interesting to be able to specify transitive relations based on previously-declared relations.  
For instance, an ACL modeling permissions on a document (reading, writing, admininistration) would benefit from
giving the writing permission to administrators, and reading permission to writers. This significantly reduces the number of tuples, as we go from:

```
doc:document1#admin@user:Théophile
doc:document1#writer@user:Théophile
doc:document1#reader@user:Théophile

doc:document1#writer@user:Léa
doc:document1#reader@user:Léa

doc:document1#reader@user:Nour
```

to
```
doc:document1#admin@user:Théophile

doc:document1#writer@user:Léa

doc:document1#reader@user:Nour
```

but the admin and writer retain their transitive permissions.

This kind of ACL check is resolved through the usage of a "rewrite rule" that expresses the inheritance of  the "reader" relationship to the document by the "writer", and that of the "writer" by the "admin".

Expanding a rewrite rule provides more users in which to search for a permission match. Thus, the final set of users is not only `readers` but `readers ∪ writers ∪ admins`.

### Indirect Hierarchical Relationships

In order to further reduce the amount of tuples stored, and lower the maintenance burden, it is also interesting to grant permissions to other relations defined elsewhere.
For instance, the `reader` relation can be defined in terms of the membership to a group:

```
doc:document1#reader@group:guests#member
```
